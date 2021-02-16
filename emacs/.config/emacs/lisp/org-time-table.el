;;; delete-class.el --- Manage time table from Org -*- lexical-binding:t -*-

;; Copyright (C) 2021  Utkarsh Singh

;; Author: Utkarsh Singh <utkarsh190601@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities to edit time table and present it in a readable Org table.

;;; Code:

(require 'org-table)

(defun org-time-table-delete-class (class)
  "Delete useless CLASS from org table cell."
  (interactive (list (read-string "Enter class: ")))
  (unless (stringp class)
    (user-error "Invalid class provided!"))
  (unless (org-at-table-p) (user-error "Not at a table"))
  
  (save-excursion
    (when (re-search-backward "|" (point-at-bol 0) t)
      (goto-char (match-end 0))
      (and (looking-at " ") (forward-char 1)))
    
    ;; TODO: use org-table-get-field
    (let ((beg (point))
	  (end (save-excursion
		 (when (re-search-forward "|" nil t)
		   (goto-char (match-end 0))
		   (backward-char 1))
		 (point))))
      (unless (re-search-forward class end t)
	(kill-region beg end)))))

(defvar class-days
  '("mon" "tue" "wed" "thu" "fri" "sat")
  "Working days of a week.")

(defun org-time-table-kill-rows (beg end)
  "Kill up empty rows from time table in region BEG and END"
  (interactive (list (region-beginning) (region-end)))
  (goto-char beg)
  (while (< (point) end)
    (if (= 0 (count-words (point-at-bol) (point-at-eol)))
	(org-table-kill-row)
      (forward-line 1))))

(defun org-time-table-clean ()
  "Clean up time table my removing all blank cells and rows."
  (interactive)
  (unless (org-at-table-p) (user-error "Not at a table"))
  (let ((table-end (save-excursion
		     (goto-char (org-table-end))
		     (org-table-previous-field)
		     (point)))
	(weekdays class-days)
	row field)
    (goto-char (org-table-begin))

    ;; iterate over table
    (while (< (point) table-end)
      (setq field (string-clean-whitespace (downcase (org-table-get-field))))

      ;; if field is as weekday consider this row
      ;; as a reference point for other row
      (when (string-equal field (car weekdays))
	(setq row (org-table-current-line))
	(setq weekdays (cdr weekdays)))

      ;; move non-empty cells to ROW
      ;; which also contains weekday
      (when (and (< 0 (length field))
		 row)
	(while (> (org-table-current-line) row)
	  (org-table-move-cell-up)))

      (org-table-next-field))

    ;; clean empty rows
    (org-time-table-kill-rows (org-table-begin) table-end)))

(provide 'org-time-table)
;;; delete-class ends here
