;;; init.el --- Utkarsh Singh Emacs Configuration  -*- lexical-binding: t; -*-
;;; Commentary:

;; A bare-boned config template.

;;; Code:

;; speed up startup
(setq gc-cons-threshold (* 50 1000 1000))

(setq inhibit-startup-screen t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

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

;; disable GUI component
(use-package emacs
  :config
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1)
  (menu-bar-mode -1))

;; to start Emacs server
(use-package server
  :hook (after-init-hook . server-start))

(use-package modus-operandi-theme
  :ensure)

(use-package modus-vivendi-theme
  :ensure)

;; theme settings
(use-package emacs
  :config
  (defmacro contrib/format-sexp (sexp &rest objects)
    `(eval (read (format ,(format "%S" sexp) ,@objects))))
  (defvar prot/modus-theme-after-load-hook nil
    "Hook that runs after loading a Modus theme.
See `prot/modus-operandi' or `prot/modus-vivendi'.")
  (dolist (theme '("operandi" "vivendi"))
    (contrib/format-sexp
     (defun prot/modus-%1$s ()
       (setq modus-%1$s-theme-slanted-constructs t
             modus-%1$s-theme-bold-constructs t
             modus-%1$s-theme-fringes nil ; {nil,'subtle,'intense}
             modus-%1$s-theme-mode-line '3d ; {nil,'3d,'moody}
	     )
       (load-theme 'modus-%1$s t)
       (run-hooks 'prot/modus-theme-after-load-hook))
     theme))
  (defun prot/modus-themes-toggle (&optional arg)
    "Toggle between `prot/modus-operandi' and `prot/modus-vivendi'.
With optional \\[universal-argument] prefix, enable
`prot/modus-themes-alt-mode' for the loaded theme."
    (interactive "P")
    (if (eq (car custom-enabled-themes) 'modus-operandi)
        (progn
          (disable-theme 'modus-operandi)
          (prot/modus-vivendi))
      (disable-theme 'modus-vivendi)
      (prot/modus-operandi)))
  :hook (after-init-hook . prot/modus-vivendi)
  :bind ("<f5>" . prot/modus-themes-toggle))

;; cache directory for emacs
(setq backup-directory-alist '(("." . "~/.cache/emacs")))

;; font settings
(use-package emacs
  :config
  (set-fontset-font t nil "Noto Color Emoji" nil 'append))

;; auto-pair
(use-package electric
  :init
  (electric-pair-mode 1))

;; parentheses highlighting
(use-package paren
  :init
  (show-paren-mode 1))

;; built-in minor mode that keeps track of the files
;; you have opened, allowing you revisit them faster.
(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25))

;; better default
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; write custom config in separate file
(use-package emacs
  :config
  (setq custom-file "~/.config/emacs/custom.el"))

;; manage other buffer with ease
(use-package emacs
  :config
  (global-set-key (kbd "C-c d") 'dired-other-window)
  (global-set-key (kbd "C-c f") 'find-file-other-window)
  ;; edit buffer with 'E'
  (global-set-key (kbd "C-c b") 'counsel-switch-buffer-other-window))

;; Deletes text under selection when insertion is made
(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

;; make Emacs prompts more tolerable
(use-package emacs
  :config
  (setq echo-keystrokes 0.25)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'overwrite-mode 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package emacs
  :config
  (defun ut/new-line-below (&optional arg)
    "Create an empty line below the current one.
Move the point to the indented area.  Adapt indentation by
passing \\[universal-argument].  Also see `ut/new-line-above'."
    (interactive "P")
    (end-of-line)
    (if arg
        (newline-and-indent)
      (newline))
    (indent-according-to-mode))

  (defun ut/new-line-above (&optional arg)
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
        (ut/new-line-below indent))))

  (defun ut/copy-line-or-region (&optional arg)
    "Kill-save the current line or active region.
With \\[universal-argument] duplicate the target instead.  When
region is active, also apply context-aware indentation while
duplicating."
    (interactive "P")
    (let* ((rbeg (region-beginning))
           (rend (region-end))
           (pbol (point-at-bol))
           (peol (point-at-eol))
           (indent (if (eq (or rbeg rend) pbol) nil arg)))
      (if arg
          (progn
            (if (use-region-p)
                (progn
                  (copy-region-as-kill rbeg rend)
                  (when (eq (point) rbeg)
                    (exchange-point-and-mark))
                  (ut/new-line-below indent))
              (copy-region-as-kill pbol peol)
              (ut/new-line-below))
            (yank))
        (copy-region-as-kill pbol peol)
        (message "Current line copied"))))


  (defun ut/yank-replace-line-or-region ()
    "Replace line or region with latest kill.
This command can then be followed by the standard
`yank-pop' (default is bound to \\[yank-pop])."
    (interactive)
    (if (use-region-p)
          (delete-region (region-beginning) (region-end))
(delete-region (point-at-bol) (point-at-eol)))
      (yank))

  (defun ut/multi-line-next ()
    "Move point 15 lines down."
    (interactive)
    (forward-line 15))

  (defun ut/multi-line-prev ()
    "Move point 15 lines up."
    (interactive)
    (forward-line -15))

  (defun ut/kill-line-backward ()
    "Kill from point to the beginning of the line."
    (interactive)
    (kill-line 0))

  ;; Based on `org--line-empty-p'.
  (defmacro ut/line-p (name regexp)
    "Make NAME function to match REGEXP on line n from point."
    `(defun ,name (n)
       (save-excursion
         (and (not (bobp))
	          (or (beginning-of-line n) t)
	          (save-match-data
	            (looking-at ,regexp))))))

  (ut/line-p
   ut/empty-line-p
   "[\s\t]*$")

  (ut/line-p
   ut/indent-line-p
   "^[\s\t]+")

  (ut/line-p
   ut/non-empty-line-p
   "^.*$")

  (ut/line-p
   ut/text-list-line-p
   "^\\([\s\t#*+]+\\|[0-9]+[).]+\\)")

  (ut/line-p
   ut/text-heading-line-p
   "^[=-]+")

  :bind (("C-S-w" . ut/copy-line-or-region)
         ("C-S-y" . ut/yank-replace-line-or-region)
         ("M-SPC" . cycle-spacing)
         ("M-o" . delete-blank-lines)   ; alias for C-x C-o
         ("M-k" . ut/kill-line-backward)
         ("C-S-n" . ut/multi-line-next)
         ("C-S-p" . ut/multi-line-prev)
         ("<C-return>" . ut/new-line-below)
         ("<C-S-return>" . ut/new-line-above)))

;; increases the selected region by semantic units
(use-package expand-region
  :ensure
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; file manager
(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode)))

;; toggle to view sub-directories in dired
(use-package dired-subtree
  :ensure
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-toggle)
	      ("<C-tab>" . dired-subtree-cycle)
	      ("<S-iso-lefttab>" . dired-subtree-remove)))

;; preview mode for Emacs
(use-package peep-dired
  :ensure
  :after dired
  :config
  (setq peep-dired-enable-on-directories nil)
  (setq peep-dired-ignored-extensions
        '("mkv" "webm" "mp4" "mp3" "ogg" "iso"))
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; use 'n' and 'p' to navigate in peed-dired mode
 (eval-after-load "peep-dired"
    '(progn
       (define-key peep-dired-mode-map (kbd "n") 'peep-dired-next-file)
       (define-key peep-dired-mode-map (kbd "p") 'peep-dired-prev-file)))

(use-package diredfl
  :ensure
  :config
  (setq diredfl-ignore-compressed-flag nil)
  :hook (dired-mode-hook . diredfl-mode))

(use-package display-line-numbers
  :config
  ;; Set absolute line numbers.  A value of "relative" is also useful.
  (setq display-line-numbers-type t)
  ;; Use absolute numbers in narrowed buffers
  (setq display-line-numbers-widen t)

  (define-minor-mode prot/display-line-numbers-mode
    "Toggle `display-line-numbers-mode' and `hl-line-mode'."
    :init-value nil
    :global nil
    (if prot/display-line-numbers-mode
        (progn
          (display-line-numbers-mode 1)
          (hl-line-mode 1)
          (setq-local truncate-lines t))
      (display-line-numbers-mode -1)
      (hl-line-mode -1)
      (setq-local truncate-lines nil)))
  :bind ("<f7>" . prot/display-line-numbers-mode))

(use-package whitespace
  :config
  (defun prot/toggle-invisibles ()
    "Toggles the display of indentation and space characters."
    (interactive)
    (if (bound-and-true-p whitespace-mode)
        (whitespace-mode -1)
      (whitespace-mode)))
  :bind (("<f6>" . prot/toggle-invisibles)
         ("C-c z" . delete-trailing-whitespace)))

;; spell checker  settings
(use-package flyspell
  :config
  (setq ispell-program-name "aspell")
  :hook ( (text-mode-hook . flyspell-mode)
	  (prog-mode-hook . flyspell-prog-mode)))

(use-package eshell
  :bind ("<s-return>" . eshell))

(use-package org
  :bind (:map org-mode-map
              ("<C-return>" . nil)
              ("<C-S-return>" . nil)))

;; mode Line setting
(column-number-mode 1)

;; reverts buffer is file is change on disk
(use-package autorevert
  :diminish
  :config
  (setq auto-revert-verbose t)
  (global-auto-revert-mode 1))

;; helps to keep mode line uncluttered
(use-package minions
  :ensure
  :config (minions-mode 1))

(use-package org-superstar
  :ensure
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; auto complete
(use-package company
  :ensure
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  :hook ((prog-mode-hook . company-mode)
	 (prog-mode-hook . company-tng-mode)))

(use-package flycheck
  :ensure
  :config
  (setq flycheck-python-pycompile-executable "python3")
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  :hook (prog-mode-hook . flycheck-mode))

(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :ensure
  :hook ((c++-mode-hook . lsp)
	 (c-mode-hook . lsp)
	 (python-mode-hook . lsp))
  :commands lsp)

;; narrowing framework
(use-package counsel
  :ensure
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view))

(use-package ace-window
  :ensure
  :init
  (global-set-key (kbd "C-x o") 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; present recency-bias in M-x command
(use-package amx
  :ensure
  :config
  (amx-mode 1))

;; try package without installing!
(use-package try
  :ensure)

(use-package with-editor
  :ensure
  :config
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor))

;; better pdf experience inside emacs
(use-package pdf-tools
  :ensure
  :config
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; swiper doesn't work in PDF-view mode
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

;; save the last position in pdf-view mode
(use-package saveplace-pdf-view
  :ensure
  :init
  (save-place-mode 1))

;; rss and atom feed reader inside emacs
(use-package elfeed
  :ensure
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/.config/emacs/elfeed/")
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-feeds
      '(("http://lukesmith.xyz/rss.xml" luke)
	("https://notrelated.libsyn.com/rss" luke)
	("https://www.archlinux.org/feeds/news/" linux distro)
	("https://ambrevar.xyz/atom.xml" emacs)
	("https://www.reddit.com/r/emacs/top.rss"emacs reddit)
	("https://www.reddit.com/r/archlinux/top.rss" linux reddit)
	("https://protesilaos.com/codelog.xml" emacs)
	("https://www.youtube.com/feeds/videos.xml?user=OmegaDungeon" linux youtube)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg" linux youtube)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" luke youtube)
	("https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" youtube)))
  (defun elfeed-v-mpv (url)
    "Watch a video from URL in MPV"
    (async-shell-command (format "mpv '%s'" url)))

  (defun elfeed-view-mpv (&optional use-generic-p)
    "Youtube-feed link"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
	       do (elfeed-untag entry 'unread)
	       when (elfeed-entry-link entry)
	       do (elfeed-v-mpv it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))
  
  (define-key elfeed-search-mode-map (kbd "v") 'elfeed-view-mpv))

;; terminal emulator inside Emacs
(use-package vterm
  :ensure
  :commands vterm
  :config
  (setq vterm-disable-bold-font nil)
  (setq vterm-disable-inverse-video nil)
  (setq vterm-disable-underline nil)
  (setq vterm-kill-buffer-on-exit nil)
  (setq vterm-max-scrollback 9999)
  (setq vterm-shell "/bin/zsh")
  (setq vterm-term-environment-variable "xterm-256color"))

;; built in process viewer inside Emacs
(use-package proced
  :commands proced
  :config
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 1)
  (setq proced-descend t)
  (setq proced-filter 'user))

;; pass(standard password manager) interface for Emacs
(use-package password-store
  :ensure
  :commands (password-store-copy
             password-store-edit
             password-store-insert)
  :config
  (setq password-store-time-before-clipboard-restore 30))

(use-package sdcv
  :ensure
  :hook (sdcv-mode-hook . (lambda ()
                            (font-lock-mode -1))))
(use-package notmuch
  :ensure)

(use-package smtpmail
  :init
  (setq smtpmail-default-smtp-server "smtp.google.com")
  :config
  (setq smtpmail-smtp-server "smtp.google.com")
  (setq smtpmail-stream-type 'ssl)
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-queue-mail nil))

(use-package smtpmail-async
  :after smtpmail
  :config
  (setq send-mail-function 'async-smtpmail-send-it)
  (setq message-send-mail-function 'async-smtpmail-send-it))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; End:
;;; init.el ends here
