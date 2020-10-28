;;; init.el --- Utkarsh Singh Emacs Configuration
;;
;;; Commentary:
;;
;; A bare-boned config template.
;;
;;; Code:

;; Speed up startup
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

;; Configure `use-package' prior to loading it.
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

;; disable GUI components
(use-package emacs
  :config
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1)
  (menu-bar-mode -1))

;; to start emacs server
(use-package server
  :hook (after-init-hook . server-start))

;;theme settings
(use-package emacs
  :config
  (defmacro contrib/format-sexp (sexp &rest objects)
    `(eval (read (format ,(format "%S" sexp) ,@objects))))

  ;; This is currently not used in this section.  Search for it in the
  ;; section about setting fonts, `prot/font-bold-face' in particular.
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

;; auto-pair
(use-package electric
  :init
  (electric-pair-mode 1))

;;parentheses highlighting
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

;; Better default
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-c d") 'dired-other-window)
(global-set-key (kbd "C-c f") 'find-file-other-window)
(global-set-key (kbd "C-c b") 'view-buffer-other-window)
(setq custom-file "~/.config/emacs/custom.el")

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

;; dired(directory editor) settings
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
  :config
  (setq diredfl-ignore-compressed-flag nil)
  :hook (dired-mode-hook . diredfl-mode))

;; spell checker  settings
(use-package flyspell
  :config
  (setq ispell-program-name "aspell")
  :hook ( (text-mode-hook . flyspell-mode)
	  (prog-mode-hook . flyspell-prog-mode)))


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

(use-package org-bullets
  :ensure
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode -1))))

;; auto complete
(use-package company
  :ensure
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (global-company-mode 1)
  (company-tng-mode))

(use-package company-irony
  :ensure
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package irony
  :ensure
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-jedi
  :ensure
  :config
  (add-hook 'python-mode-hook 'jedi:setup))

(defun ut/python-mode-hook ()
  "Add company-back-end and company-jedi to python major mode."
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'ut/python-mode-hook)

(use-package company-quickhelp
  :ensure
  :init
  (company-quickhelp-mode))

(use-package irony-eldoc
  :ensure
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package flycheck
  :ensure
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-python-pycompile-executable "python3")
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; (setq lsp-keymap-prefix "C-c l")

;; (use-package lsp-mode
;;   :ensure
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (c++-mode-hook . lsp)
;; 	 (python-mode-hook . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration)
;; 	 )
;;   :config
;;   :commands lsp)

;; ;; lsp and helm integration
;; (use-package helm-lsp
;;   :ensure
;;   :commands helm-lsp-workspace-symbol)

;; narrowing framework
(use-package helm
  :ensure
  :init
  (helm-mode 1)
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  ;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

;; try package without installing!!
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
  ;; midnight mode to be coherent with modus-vivendi theme
  (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))
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
	("https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" youtube))))

(use-package bongo
  :ensure )

;; pass(standard password manager) interface for Emacs
(use-package password-store
  :ensure
  :commands (password-store-copy
             password-store-edit
             password-store-insert)
  :config
  (setq password-store-time-before-clipboard-restore 30))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
