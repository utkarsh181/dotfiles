;;; utkarsh-elfeed.el --- Elfeed extensions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021  Utkarsh Singh

;; Author: Utkarsh Singh <utkarsh190601@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(when (featurep 'vterm)
  (require 'vterm))
(require 'cl-lib)

(defcustom vterm-buffer-name "*vterm*"
  "The basename used for vterm buffers.
This is the default name used when running `eshell'.

With a numeric prefix argument to `vterm', the buffer name will
be the value of this variable followed by the number.  For
example, with the numeric prefix argument 2, the buffer would be
named \"*vterm*<2>\"."
  :type 'string
  :group 'utkarsh-vterm)

(defun utkarsh-vterm (&optional arg)
  "Create an interactive Vterm buffer.
Start a new Vterm session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Vterm sessions is determined by the
value of `vterm-buffer-name', which see."
  (interactive "P")
  (cl-assert vterm-buffer-name)
  (let ((buf (cond ((numberp arg)
		    (get-buffer-create (format "%s<%d>"
					       vterm-buffer-name
					       arg)))
		   (arg
		    (generate-new-buffer vterm-buffer-name))
		   (t
		    (get-buffer-create vterm-buffer-name)))))
    (cl-assert (and buf (buffer-live-p buf)))
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'vterm-mode)
      (vterm-mode))
    buf))
