;;; prot-simple.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

;; Copyright (c) 2020-2021  Utkarsh Singh <utkarsh190601@gmail.com>

;; Author: Utkarsh Singh <utkarsh190601@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Common commands for my Emacs
;; Most of this file is taken from Prot's config:
;; <https://protesilaos.com/dotemacs/>.

;;; Code:

(defun utkarsh-new-line-below (&optional arg)
  "Create an empty line below the current one.
Move the point to the indented area.  Adapt indentation by
passing \\[universal-argument].  Also see `new-line-above'."
  (interactive "P")
  (end-of-line)
  (if arg
      (newline-and-indent)
    (newline))
  (indent-according-to-mode))

(defun utkarsh-new-line-above (&optional arg)
  "Create an empty line above the current one.
Move the point to the absolute beginning.  Adapt indentation by
passing \\[universal-argument]."
  (interactive "P")
  (let ((indent (if arg arg nil)))
    (if (or (bobp)
            (line-number-at-pos 1))
        (progn
          (beginning-of-line)
          (newline)
          (forward-line -1))
      (forward-line -1)
      (new-line-below indent))))

(defun utkarsh-multi-line-next ()
  "Move point 15 lines down."
  (interactive)
  (forward-line 15))

(defun utkarsh-multi-line-prev ()
  "Move point 15 lines up."
  (interactive)
  (forward-line -15))

(defun utkarsh-kill-line-backward ()
  "Kill from point to the beginning of the line."
  (interactive)
  (kill-line 0))
