;;; init.el --- Personal configuration file -*- lexical-binding: t -*-

;; Copyright (c) 2020-2021  Utkarsh Singh <utkarsh190601@gmail.com>

;; Author: Utkarsh Singh <utkarsh190601@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See my dotfiles: https://github.com/utkarsh181/dotfiles

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(add-hook 'package-menu-mode-hook #'hl-line-mode)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

;; my custom library
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;; to start emacs server
(use-package server
  :hook (after-init-hook . (lambda ()
			     (unless (server-running-p)
			       (server-start)))))

;; basic settings
(use-package emacs
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (put 'narrow-to-region 'disabled nil)
  (put 'overwrite-mode 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'upcase-region 'disabled nil)
  :custom
  (echo-keystrokes 0.25))

;; theme settings
(use-package modus-themes
  :ensure t
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

;; narrowing framework
(use-package counsel
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (counsel-switch-buffer-preview-virtual-buffers nil))

;; present recency-bias in M-x command
(use-package amx
  :ensure t
  :config
  (amx-mode 1))

;; font settings
(use-package emacs
  :config
  (set-face-font 'default "DejaVu Sans Mono-11")
  (set-fontset-font t nil "Noto Color Emoji" nil 'append))

;; auto-pair
(use-package electric
  :config
  (electric-pair-mode 1))

;; parentheses highlighting
(use-package paren
  :config
  (show-paren-mode 1))

;; built-in minor mode that keeps track of the files
;; you have opened, allowing you revisit them faster.
(use-package recentf
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25))

(use-package emacs
  :custom
  ;; apropos sort by relevancy
  (apropos-sort-by-scores t)
  :bind (("C-x C-b" . ibuffer)
	 ("M-z" . zap-up-to-char)))

(use-package emacs
  :custom
  (backup-directory-alist '(("." . "~/.cache/emacs")))
  (custom-file (concat user-emacs-directory "custom.el")))

;; manage other buffer with ease
(use-package emacs
  :bind (("C-c d" . dired-other-window)
	 ("C-c f" . find-file-other-window)
	 ("C-c b" . switch-to-buffer-other-window)))

;; deletes text under selection when insertion is made
(use-package delsel
  :config
  (delete-selection-mode 1))

;; custom key bindings to reduce keystrokes for regular editing commands
(use-package utkarsh-simple
  :config
  :bind (("M-SPC" . cycle-spacing)     ; activate widow menu in gnome-shell
         ("M-o" . delete-blank-lines)   ; alias for C-x C-o
	 ("M-=" . count-words)
         ("M-k" . utkarsh-kill-line-backward)
         ("C-S-n" . utkarsh-multi-line-next)
         ("C-S-p" . utkarsh-multi-line-prev)
         ("<C-return>" . utkarsh-new-line-below)
         ("<C-S-return>" . utkarsh-new-line-above)))

;; Increases The selected region by semantic units
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; control version-control from Emacs
(use-package vc
  :custom
  (vc-follow-symlinks t))

;; directory editor
(use-package dired
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode)))

(use-package dired-x
  :after dired
  :bind (("C-x C-j" . dired-jump)
	 ("C-c C-j" . dired-jump-other-window)))

;; preview mode for dired
(use-package peep-dired
  :ensure t
  :after dired
  :custom
  (peep-dired-enable-on-directories nil)
  (peep-dired-ignored-extensions
   '("mkv" "webm" "mp4" "mp3" "ogg" "iso"))
  :bind (:map dired-mode-map
	      ("P" . peep-dired)
	      ;; use 'n' and 'p' to navigate in peep-dired mode
	      :map peep-dired-mode-map
	      ("n" . peep-dired-next-file)
	      ("p" . peep-dired-prev-file)))

;; make dired more colourful
(use-package diredfl
  :ensure t
  :custom
  (diredfl-ignore-compressed-flag nil)
  :hook (dired-mode-hook . diredfl-mode))

(use-package dired-aux
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t))

(use-package isearch
  :diminish
  :custom
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  (isearch-lazy-count t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " (%s/%s)")
  :bind (:map isearch-mode-map
	      ("C-g" . isearch-cancel)))

(use-package display-line-numbers
  :config
  (define-minor-mode line-number-and-hl-mode
    "Toggle `display-line-numbers-mode' and `hl-line-mode'."
    :init-value nil
    :global nil
    (if line-number-and-hl-mode
        (progn
          (display-line-numbers-mode 1)
          (hl-line-mode 1)
          (setq-local truncate-lines t))
      (display-line-numbers-mode -1)
      (hl-line-mode -1)
      (setq-local truncate-lines nil)))
  :custom
  ;; set absolute line numbers
  (display-line-numbers-type t)
  ;; use absolute numbers in narrowed buffers
  (display-line-numbers-widen t)
  :bind ("<f7>" . line-number-and-hl-mode))

(use-package whitespace
  :config
  (defun toggle-invisibles ()
    "Toggles the display of indentation and space characters."
    (interactive)
    (if (bound-and-true-p whitespace-mode)
        (whitespace-mode -1)
      (whitespace-mode)))
  :bind (("<f6>" . toggle-invisibles)
         ("C-c z" . delete-trailing-whitespace)))

;; spell checker  settings
(use-package flyspell
  :custom
  (ispell-program-name "aspell")
  :hook ((text-mode-hook . flyspell-mode)
	 (prog-mode-hook . flyspell-prog-mode)))

(use-package org
  :custom
  (org-catch-invisible-edits 'show)
  (org-log-done 'time)
  :bind (("C-c l" . org-store-link) ; may conflict with lsp-key-prefix!
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 :map org-mode-map
	 ("<C-return>" . nil)
	 ("<C-S-return>" . nil)))

;; display column number in mode line
(use-package emacs
  :config
  (column-number-mode 1))

;; reverts buffer is file is change on disk
(use-package autorevert
  :diminish
  :config
  (global-auto-revert-mode 1)
  :custom
  (auto-revert-verbose t))

;; helps to keep mode line uncluttered
(use-package minions
  :ensure t
  :config
  (minions-mode 1))

(use-package abbrev
  :bind ("C-x a u" . unexpand-abbrev))

;; text completion framework
(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  :hook ((prog-mode-hook . company-mode)
	 ;; use <tab> to cycle through completion
	 (prog-mode-hook . company-tng-mode)))

(use-package flycheck
  :ensure t
  :custom
  (flycheck-python-pycompile-executable "python3")
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  :hook ((c++-mode-hook . flycheck-mode)
	 (c-mode-hook . flycheck-mode)
	 (python-mode-hook . flycheck-mode)))

;; language server mode
(use-package lsp-mode
  :ensure t
  :init
  ;; (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-before-save-edits nil)
  (lsp-headerline-breadcrumb-enable nil)
  :hook ((c++-mode-hook . lsp)
	 (c-mode-hook . lsp)
	 (python-mode-hook . lsp))
  :commands lsp)

;; elisp live documentation feedback
(use-package eldoc
  :diminish
  :config
  (global-eldoc-mode 1))

;; manage how Emacs uniquely define identical-named files
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t))

;; undo system for window management
(use-package winner
  :init
  (winner-mode 1)
  :bind (("<C-right>" . winner-redo)
         ("<C-left>" . winner-undo)))

;; use ctrl-super-vim_keys to move around windows
(use-package windmove
  :custom
  (windmove-create-window nil)
  :bind (("C-s-k" . windmove-up)
         ("C-s-l" . windmove-right)
         ("C-s-j" . windmove-down)
         ("C-s-h" . windmove-left)
         ;; numpad keys clockwise: 8 6 2 4
         ("<kp-up>" . windmove-up)
         ("<kp-right>" . windmove-right)
         ("<kp-down>" . windmove-down)
         ("<kp-left>" . windmove-left)))

;; try package without installing!
(use-package try
  :ensure t)

;; use the Emacsclient as $EDITOR
(use-package with-editor
  :ensure t
  :hook ((eshell-mode-hook . with-editor-export-editor)
	 (shell-mode-hook . with-editor-export-editor)
	 (term-mode-hook . with-editor-export-editor)))

;; better pdf experience inside emacs
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install)
  :custom
  ;; open pdfs scaled to fit page
  (pdf-view-display-size 'fit-page)
  :bind (:map pdf-view-mode-map
	      ("C-s" . isearch-forward)))

;; save the last position in pdf-view mode
(use-package saveplace-pdf-view
  :ensure t
  :init
  (save-place-mode 1))

;; rss and atom feed reader inside emacs
(use-package elfeed
  :ensure t
  :custom
  (elfeed-use-curl t)
  (elfeed-curl-max-connections 10)
  (elfeed-db-directory (concat user-emacs-directory "elfeed/"))
  (elfeed-enclosure-default-dir "~/Downloads/")
  (elfeed-search-clipboard-type 'CLIPBOARD)
  (elfeed-show-truncate-long-urls t)
  (elfeed-show-unique-buffers t)
  :bind (:map elfeed-search-mode-map
	      ("v" . utkarsh-elfeed-mpv-dwim)
	      :map elfeed-show-mode-map
	      ("v" . utkarsh-elfeed-mpv-dwim)
	      ("e" . utkarsh-elfeed-show-eww)))

;; store feed in gpg encrypted file
(use-package utkarsh-elfeed
  :hook (elfeed-search-mode-hook . utkarsh-elfeed-load-feeds))

;; shell implemented in elisp
(use-package eshell
  :custom
  (eshell-prefer-lisp-function t)
  :bind ("<s-return>" . eshell))

(use-package esh-mode
  :bind (:map eshell-mode-map
	      ("M-k" . eshell-kill-input)))

(use-package esh-module
  :config
  (setq eshell-modules-list
        '(eshell-alias
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          eshell-term
          eshell-tramp
          eshell-unix)))

;; cache password for 10 mins
(use-package em-tramp
  :after esh-mode
  :custom
  (password-cache t)
  (password-cache-expiry 600))

;; terminal emulator inside Emacs though eshell just works
(use-package vterm
  :ensure t
  :commands vterm
  :custom
  (vterm-disable-bold nil)
  (vterm-disable-inverse-video nil)
  (vterm-disable-underline nil)
  (vterm-kill-buffer-on-exit nil)
  (vterm-max-scrollback 9999)
  (vterm-shell "/bin/zsh")
  (vterm-term-environment-variable "xterm-256color"))

;; built in process viewer inside Emacs
(use-package proced
  :commands proced
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-descend t)
  (proced-filter 'user))

;; Emacs interface for pass(standard password manager)
(use-package password-store
  :ensure t
  :custom
  (password-store-time-before-clipboard-restore 30)
  :commands (password-store-copy
             password-store-edit
             password-store-insert))

;; local dictionary using sdcv
(use-package sdcv
  :ensure t
  ;; remove font-lock which causes awkward highlighting
  :hook (sdcv-mode-hook . (lambda ()
                            (font-lock-mode -1))))
(use-package sendmail
  :custom
  (user-mail-address "utkarsh190601@gmail.com")
  (user-full-name "Utkarsh Singh")
  (mail-signature "Utkarsh Singh\n"))

(use-package message
  :config
  (defun message-recipients ()
    "Return a list of all recipients in the message, looking at TO, CC and BCC.
Each recipient is in the format of `mail-extract-address-components'."
    (mapcan (lambda (header)
              (let ((header-value (message-fetch-field header)))
		(and
		 header-value
		 (mail-extract-address-components header-value t))))
            '("To" "Cc" "Bcc")))

  (defun message-all-epg-keys-available-p ()
    "Return non-nil if the pgp keyring has a public key for each recipient."
    (require 'epa)
    (let ((context (epg-make-context epa-protocol)))
      (catch 'break
	(dolist (recipient (message-recipients))
          (let ((recipient-email (cadr recipient)))
            (when (and recipient-email (not (epg-list-keys context recipient-email)))
              (throw 'break nil))))
	t)))

  (defun message-sign-encrypt-if-all-keys-available ()
    "Add MML tag to encrypt message when there is a key for each recipient.
Consider adding this function to `message-send-hook' to
systematically send encrypted emails when possible."
    (when (message-all-epg-keys-available-p)
      (mml-secure-message-sign-encrypt)))
  :custom
  (mail-user-agent 'message-user-agent)
  (message-signature "Utkarsh Singh\n")
  (message-kill-buffer-on-exit t)
  (message-directory "~/.local/share/mail")
  (mml-secure-openpgp-sign-with-sender t)
  :hook ((message-setup-hook . mml-secure-message-sign-pgpmime)
	 (message-send-hook . message-sign-encrypt-if-all-keys-available)))

;; send mail from inside Emacs using smtp protocol
(use-package smtpmail
  :custom
  (sendmail-program "/usr/bin/msmtp")
  (send-mail-function 'smtpmail-send-it)
  (message-sendmail-f-is-evil t)
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (message-send-mail-function 'message-send-mail-with-sendmail))

;; manage mail inside emacs
;; with powerful search features and
;; effective tag sorting
(use-package notmuch
  :load-path "/usr/share/emacs/site-lisp"
  :custom
  (notmuch-search-oldest-first nil)
  (notmuch-fcc-dirs '("utkarsh190601@gmail.com" . "utkarsh190601@gmail.com/[Gmail].Sent +sent -inbox"))
  (notmuch-fcc-dirs
      '(("utkarsh190601@gmail.com" . "utkarsh190601@gmail.com/[Gmail].Sent +sent -inbox -unread")))
  (notmuch-archive-tags '("-inbox" "-unread" "+deleted"))
  (notmuch-crypto-process-mime t))

;; music client
(use-package emms
  :ensure t
  :config
  (emms-all)
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  :custom
  ;; emms as standalone client
  (emms-player-list '(emms-player-mpv))
  (emms-source-file-default-directory "~/Music/")
  ;; emms to work with mpd
  (emms-player-mpd-server-name "localhost")
  (emms-player-mpd-server-port "6600")
  (emms-player-mpd-music-directory "~/Music"))

(use-package gnus
  :custom
  (gnus-select-method
	'(nntp "news.gwene.org")))

(use-package shr
  :custom
  (shr-use-colors nil))

;; mount umount usb and android from Emacs!
(use-package mount-umount)

;; End:
;;; init.el ends here

(put 'downcase-region 'disabled nil)
