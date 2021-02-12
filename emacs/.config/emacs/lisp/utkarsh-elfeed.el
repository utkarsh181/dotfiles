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

;;; Commentary:
;;
;; Extensions for Elfeed, intended for use in my Emacs setup:

;;; Code:

(eval-when-compile (require 'subr-x))
(when (featurep 'elfeed)
  (require 'elfeed))

(defcustom prot-elfeed-feeds-file (concat user-emacs-directory "feeds.el.gpg")
  "Path to file with `elfeed-feeds'."
  :type 'string
  :group 'prot-elfeed)

(defun utkarsh-elfeed-load-feeds ()
  "Load file containing the `elfeed-feeds' list.
Add this to `elfeed-search-mode-hook'."
  (let ((feeds prot-elfeed-feeds-file))
    (if (file-exists-p feeds)
        (load-file feeds)
      (user-error "Missing feeds' file"))))
