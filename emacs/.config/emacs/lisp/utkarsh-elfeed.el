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
;; Most of this file is takes from Prot's dotfiles.
;; Extensions for Elfeed, intended for use in my Emacs setup.

;;; Code:

(eval-when-compile (require 'subr-x))
(when (featurep 'elfeed)
  (require 'elfeed))

(defgroup utkarsh-elfeed ()
  "Personal extension for Elfeed"
  :group 'elfeed)

(defcustom utkarsh-elfeed-feeds-file (concat user-emacs-directory "feeds.el.gpg")
  "Path to file with `elfeed-feeds'."
  :type 'string
  :group 'utkarsh-elfeed)

(defun utkarsh-elfeed-load-feeds ()
  "Load file containing the `elfeed-feeds' list.
Add this to `elfeed-search-mode-hook'."
  (let ((feeds utkarsh-elfeed-feeds-file))
    (if (file-exists-p feeds)
        (load-file feeds)
      (user-error "Missing feeds' file"))))

(defun utkarsh-elfeed-show-eww (&optional link)
  "Browse current entry's link or optional LINK in `eww'.

Only show the readable part once the website loads.  This can
fail on poorly-designed websites."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (or link (elfeed-entry-link entry))))
    (eww link)
    (add-hook 'eww-after-render-hook 'eww-readable nil t)))

(defvar utkarsh-elfeed-mpv-buffer-name "*utkarsh-elfeed-mpv-output*"
  "Name of buffer holding Elfeed MPV output.")

(defun utkarsh-elfeed--get-mpv-buffer ()
  "Prepare `utkarsh-elfeed-mpv-buffer-name' buffer."
  (let ((buf (get-buffer utkarsh-elfeed-mpv-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer))))

(declare-function elfeed-entry-enclosures "elfeed")

(defun utkarsh-elfeed-mpv-dwim ()
  "Play entry link with the external MPV program.
When there is an audio enclosure (assumed to be a podcast), play
just the audio.  Else spawn a video player at a resolution that
accounts for the current monitor's width."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (concat "'" (elfeed-entry-link entry) "'"))
         (buf (pop-to-buffer utkarsh-elfeed-mpv-buffer-name)))
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (utkarsh-elfeed--get-mpv-buffer)
    (async-shell-command (format "mpv %s" link) buf)
    (message "Launching MPV for %s" link)))
