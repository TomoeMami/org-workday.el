;;; org-workday.el --- Snap repeated Org tasks onto workdays  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 TomoeMami

;; Author: TomoeMami <Trembleafterme@Outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-pro
;; Maintainer: TomoeMami <Trembleafterme@Outlook.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.0") (org-repeat-by-cron "1.1"))
;; Keywords: calendar, org, convenience
;; URL: https://github.com/TomoeMami/org-workday.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-workday knows which days are workdays, taking weekends, public
;; holidays and make-up workdays into account, and snaps the
;; SCHEDULED/DEADLINE timestamps of repeated Org tasks onto workdays.
;;
;; Holidays and make-up workdays are declared as diary sexp entries in
;; Org agenda files (see `org-workday-calendar-files').  An entry
;; whose text contains `org-workday-workday-marker' declares a make-up
;; workday; one containing `org-workday-holiday-marker' declares a
;; holiday.  For example:
;;
;;   * Holidays
;;   %%(diary-date 1 1 2026 t) 🏮 New Year's Day
;;   %%(diary-date 2 21 2026 t) 💼 Make-up workday
;;
;; After a task managed by org-repeat-by-cron is marked DONE, if the
;; task carries the `org-workday-tag' tag, its timestamp is moved
;; according to its WORKDAY_DIRECTION and WORKDAY_OFFSET properties.
;;
;; Usage (recommended):
;;
;;   (global-org-workday-mode)
;;
;; Or, without the minor mode:
;;
;;   (add-hook 'org-repeat-by-cron-after-repeat-functions
;;             #'org-workday-snap-to-workday)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-agenda)
(require 'org-repeat-by-cron)

(defgroup org-workday nil
  "Snap repeated Org tasks onto workdays."
  :group 'org
  :prefix "org-workday-")

(defcustom org-workday-calendar-files nil
  "Files whose diary sexp entries declare holidays and make-up workdays.

When nil, use the files in `org-agenda-files' (expanded as by the
function of the same name, which turns directories into the Org
files they contain).  The value may be a single file name or a list
of file names.  On any given day, every matching diary sexp entry in
these files is inspected: an entry containing
`org-workday-workday-marker' declares a make-up workday, and an
entry containing `org-workday-holiday-marker' declares a holiday."
  :type '(choice (const :tag "Use `org-agenda-files'" nil)
                 (file :tag "Single file")
                 (repeat :tag "List of files" file)))

(defcustom org-workday-workday-marker "💼"
  "String marking a diary sexp entry as a make-up workday."
  :type 'string)

(defcustom org-workday-holiday-marker "🏮"
  "String marking a diary sexp entry as a holiday."
  :type 'string)

(defcustom org-workday-weekend-days nil
  "Days of the week treated as non-working days.

A list of numbers where 0 is Sunday and 6 is Saturday.  When nil,
`org-agenda-weekend-days' is used instead."
  :type '(choice (const :tag "Use `org-agenda-weekend-days'" nil)
                 (set :tag "Weekend days"
                      (const :tag "Sunday" 0)
                      (const :tag "Monday" 1)
                      (const :tag "Tuesday" 2)
                      (const :tag "Wednesday" 3)
                      (const :tag "Thursday" 4)
                      (const :tag "Friday" 5)
                      (const :tag "Saturday" 6))))

(defcustom org-workday-search-limit 30
  "Maximum number of days searched in either direction.

Used by `org-workday-find-workday-date'."
  :type 'integer)

(defcustom org-workday-tag "工作"
  "Tag of headings whose timestamps are snapped to workdays.

Used by `org-workday-snap-to-workday'."
  :type 'string)

(defcustom org-workday-direction-prop "WORKDAY_DIRECTION"
  "Heading property overriding the default snapping direction.

See `org-workday-find-workday-date' for the direction syntax."
  :type 'string)

(defcustom org-workday-offset-prop "WORKDAY_OFFSET"
  "Heading property overriding the default snapping offset.

The value must be an integer; positive moves towards the future."
  :type 'string)

(defun org-workday--calendar-files ()
  "Return the existing files scanned for workday/holiday entries."
  (let ((files (or org-workday-calendar-files
                   (org-agenda-files 'unrestricted))))
    (cond ((null files) nil)
          ((stringp files)
           (and (file-exists-p files)
                (not (file-directory-p files))
                (list files)))
          ((listp files)
           (cl-remove-if-not
            (lambda (f)
              (and (file-exists-p f) (not (file-directory-p f))))
            files))
          (t (user-error "Invalid `org-workday-calendar-files': %S" files)))))

(defun org-workday--date-string-to-gregorian (date)
  "Parse DATE, a \"YYYY-MM-DD\" string, into a (month day year) list."
  (let ((parts (mapcar #'string-to-number (split-string date "-"))))
    (unless (= 3 (length parts))
      (error "Invalid org-workday date string: %S" date))
    (list (nth 1 parts) (nth 2 parts) (nth 0 parts))))

(defun org-workday--cron-rule-p (rule)
  "Return non-nil when RULE has the shape of a cron expression.

A cron expression has three or five space-separated fields, matching
the behavior of `org-repeat-by-cron--cron-rule-arity' in
org-repeat-by-cron 1.1.11 (reimplemented here to avoid depending on
an internal function)."
  (when rule
    (let ((n (length (split-string (string-trim rule) "[ \t]+" t))))
      (memq n '(3 5)))))

;;;###autoload
(defun org-workday-date-is-workday-p (date &optional offset)
  "Return non-nil if DATE is a workday, looking OFFSET days away.

DATE is a string in \"YYYY-MM-DD\" format.  OFFSET is a number of
days relative to DATE; positive moves towards the future.

A day is a workday when it is declared a make-up workday (a diary
sexp entry containing `org-workday-workday-marker'), or when it is
neither a holiday (an entry containing `org-workday-holiday-marker')
nor a weekend day (see `org-workday-weekend-days')."
  (let* ((greg (org-workday--date-string-to-gregorian date))
         (target (calendar-gregorian-from-absolute
                  (+ (calendar-absolute-from-gregorian greg) (or offset 0))))
         (entries (cl-loop for file in (org-workday--calendar-files)
                           nconc (org-agenda-get-day-entries file target :sexp)))
         (text (mapconcat #'identity entries " "))
         (make-up (string-match-p (regexp-quote org-workday-workday-marker) text))
         (holiday (string-match-p (regexp-quote org-workday-holiday-marker) text))
         (dow (calendar-day-of-week target)))
    (cond (make-up t)
          (holiday nil)
          (t (not (member dow (or org-workday-weekend-days
                                  org-agenda-weekend-days)))))))

;;;###autoload
(defun org-workday-find-workday-date (date-str &optional direction offset)
  "Return the nearest workday relative to DATE-STR, plus OFFSET days.

DATE-STR is a string in \"YYYY-MM-DD\" format.  DIRECTION is a
string with the following syntax:

- \"-\" (the default) searches towards the past, \"+\" towards the
  future;
- an optional boundary word selects a day at the edge of a run of
  consecutive workdays: \"start\" selects the first day of the run
  and \"end\" selects the last one;
- combined forms such as \"-start\", \"+end\", \"-end\" and
  \"+start\" are supported.

OFFSET is an integer number of days applied after the search;
positive moves towards the future.  The search stops after
`org-workday-search-limit' days; nil is returned when no workday is
found."
  (let* ((dir-str (if (stringp direction) (downcase (string-trim direction)) ""))
         (step (if (string-match-p "\\+" dir-str) 1 -1))
         (boundary (cond ((string-match-p "start" dir-str) 'start)
                         ((string-match-p "end" dir-str) 'end)
                         (t nil)))
         (offset (cond ((numberp offset) offset)
                       ((and (stringp offset)
                             (not (string-empty-p (string-trim offset))))
                        (string-to-number (string-trim offset)))
                       (t 0)))
         (found nil)
         (i 0))
    (while (and (<= i org-workday-search-limit) (null found))
      (let ((candidate (* step i)))
        (when (org-workday-date-is-workday-p date-str candidate)
          (setq found
                (pcase boundary
                  ('start
                   (unless (org-workday-date-is-workday-p date-str (1- candidate))
                     candidate))
                  ('end
                   (unless (org-workday-date-is-workday-p date-str (1+ candidate))
                     candidate))
                  (_ candidate)))))
      (cl-incf i))
    (when found
      (let* ((greg (org-workday--date-string-to-gregorian date-str))
             (result (calendar-gregorian-from-absolute
                      (+ (calendar-absolute-from-gregorian greg) found offset))))
        (format "%04d-%02d-%02d" (nth 2 result) (nth 0 result) (nth 1 result))))))

(defun org-workday--snap-timestamp-to-workday (type &optional direction offset)
  "Snap the TYPE timestamp of the entry at point to a workday.

TYPE is \"SCHEDULED\" or \"DEADLINE\".  DIRECTION and OFFSET are
passed to `org-workday-find-workday-date'.  A time of day in the
timestamp is preserved."
  (let ((ts-str (org-entry-get nil type)))
    (when (and ts-str (not (string-empty-p (string-trim ts-str))))
      (let* ((time (org-time-string-to-time ts-str))
             (date-str (format-time-string "%Y-%m-%d" time))
             (has-time (string-match-p "[0-9]\\{1,2\\}:[0-9]\\{2\\}" ts-str))
             (new-date (org-workday-find-workday-date date-str direction offset)))
        (when (and new-date (not (string= new-date date-str)))
          (let ((new-str (if has-time
                             (format "%s %s" new-date
                                     (format-time-string "%H:%M" time))
                           new-date)))
            (pcase type
              ("SCHEDULED" (org-schedule nil new-str))
              ("DEADLINE" (org-deadline nil new-str)))))))))

;;;###autoload
(defun org-workday-snap-to-workday ()
  "Snap the repeated task at point onto a workday.

Intended for `org-repeat-by-cron-after-repeat-functions'.  When the
current heading carries the `org-workday-tag' tag, its timestamp is
moved according to the `org-workday-direction-prop' and
`org-workday-offset-prop' properties.  Defaults: towards the past,
offset 0.

Which timestamps are moved mirrors `org-repeat-by-cron-on-done':

- REPEAT_DEADLINE = t               → DEADLINE only
- REPEAT_DEADLINE = cron expression → DEADLINE and SCHEDULED
- anything else                     → SCHEDULED only"
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (org-back-to-heading t)
      (when (member org-workday-tag (org-get-tags))
        (let* ((direction (org-entry-get nil org-workday-direction-prop))
               (offset-str (org-entry-get nil org-workday-offset-prop))
               (offset (if (and offset-str
                                (not (string-empty-p (string-trim offset-str))))
                           (string-to-number (string-trim offset-str))
                         0))
               (deadline-prop (org-entry-get nil org-repeat-by-cron-deadline-prop))
               (process-deadline nil)
               (process-schedule nil))
          (cond
           ((and deadline-prop (string= deadline-prop "t"))
            (setq process-deadline t))
           ((org-workday--cron-rule-p deadline-prop)
            (setq process-deadline t)
            (setq process-schedule t))
           (t (setq process-schedule t)))
          (when process-schedule
            (org-workday--snap-timestamp-to-workday
             "SCHEDULED" direction offset))
          (when process-deadline
            (org-workday--snap-timestamp-to-workday
             "DEADLINE" direction offset)))))))

;;;###autoload
(define-minor-mode global-org-workday-mode
  "Toggle snapping of repeated Org tasks onto workdays.

This is a global minor mode.  When enabled, it adds
`org-workday-snap-to-workday' to
`org-repeat-by-cron-after-repeat-functions'."
  :global t
  :group 'org-workday
  (if global-org-workday-mode
      (add-hook 'org-repeat-by-cron-after-repeat-functions
                #'org-workday-snap-to-workday)
    (remove-hook 'org-repeat-by-cron-after-repeat-functions
                 #'org-workday-snap-to-workday)))

(provide 'org-workday)

;;; org-workday.el ends here
