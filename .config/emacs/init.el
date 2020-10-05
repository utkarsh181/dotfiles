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
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

;; disable GUI components
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(menu-bar-mode -1)

;; (add-to-list 'default-frame-alist '(font . "JetBrains Mono Medium:size=12"))
;; (set-face-attribute 'default t :font "JetBrains Mono Medium:size=12")

;; theme setting
(use-package modus-operandi-theme
  :ensure t
  :config
  (setq modus-operandi-theme-visible-fringe t
	modus-operandi-theme-3d-modeline t
	modus-operandi-theme-bold-constructs t
	modus-operandi-theme-slanted-constructs t)
  :init
  ;; (load-theme 'modus-operandi t)
  )

(use-package modus-vivendi-theme
  :ensure t
  :config
  ;; enable some of the customisation options before loading the theme
  (setq modus-vivendi-theme-visible-fringe t
	modus-Vivendi-theme-3d-modeline t
	modus-vivendi-theme-bold-constructs t
	modus-vivendi-theme-slanted-constructs t
	)
  :init
  ;; load the theme
  (load-theme 'modus-vivendi t))

;; cache directory for emacs
(setq backup-directory-alist '(("." . "~/.cache/emacs")))

;; auto-pair and parentheses highlighting
(electric-pair-mode 1)
(show-paren-mode 1)


(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

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

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; Dired setting
(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode)))

;; toggle to view sub-directories in Dired
(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-toggle)
	      ("<C-tab>" . dired-subtree-cycle)
	      ("<S-iso-lefttab>" . dired-subtree-remove)))
;; toggle to peep in dired
(use-package peep-dired
  :ensure t
  :after dired
  :config
  (setq peep-dired-enable-on-directories nil)
  (setq peep-dired-ignored-extensions
        '("mkv" "webm" "mp4" "mp3" "ogg" "iso"))
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; because author was a vimmer
 (eval-after-load "peep-dired"
    '(progn
       (define-key peep-dired-mode-map (kbd "n") 'peep-dired-next-file)
       (define-key peep-dired-mode-map (kbd "p") 'peep-dired-prev-file)))

(use-package diredfl
  :ensure
  :config
  (setq diredfl-ignore-compressed-flag nil)
  :hook (dired-mode-hook . diredfl-mode))

;; flyspell settings
(use-package flyspell
  :config
  (setq ispell-program-name "aspell")
  :hook ( (text-mode-hook . flyspell-mode)
	  (prog-mode-hook . flyspell-prog-mode)))


;; Mode Line setting
(column-number-mode 1)

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (global-company-mode t)
  (company-tng-mode))

(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup))

(defun ut/python-mode-hook ()
  "Add company-back-end and company-jedi to python major mode."
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'ut/python-mode-hook)

(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode))

(use-package irony-eldoc
  :ensure t
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-python-pycompile-executable "python3")
  (add-hook 'after-init-hook #'global-flycheck-mode))

(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :ensure t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c++-mode-hook . lsp)
	 (python-mode-hook . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
	 )
  :config
  :commands lsp)

;; lsp and helm integration
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred
(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)))

(use-package helm
  :ensure t
  :config
  (helm-mode)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-r") 'helm-recentf))

(use-package helm-flyspell
  :ensure t
  :config
  ;; (define-key evil-normal-state-map (kbd "z=") 'helm-flyspell-correct))
  (define-key flyspell-mode-map (kbd "C-c $") 'helm-flyspell-correct ))

(use-package pdf-tools
  :ensure t
  ;; :pin manual ;; manually update
  :config
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; use normal isearch
  (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))
  

(use-package saveplace-pdf-view
  :ensure t
  :init
  (save-place-mode 1))

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory "~/.config/emacs/elfeed/")
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds
      '("http://lukesmith.xyz/rss.xml"
	"https://notrelated.libsyn.com/rss"
	"https://www.archlinux.org/feeds/news/"
	"https://ambrevar.xyz/atom.xml"
	"https://protesilaos.com/codelog.xml")))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
