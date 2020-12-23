;; Set up package management
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")

;; Set up use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;; set path
(add-to-list 'exec-path "~/.local/bin")
(add-to-list 'exec-path "~/.cargo/bin")

;; set default directory
(setq default-directory "~/")
(setq command-line-default-directory "~/")

;; set shell to zsh
(setq shell-file-name "/bin/zsh")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET DEFAULT CONFIGURATIONS         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set boolean to check if we're on the personal computer, or the work computer
(defconst is-personal-computer (if (string-equal (getenv "USER") "zsilver4") nil t))

(defconst is-mac (eq system-type 'darwin))

;; set default python interpreter
(defconst python-interpreter "python")
(setq python-shell-interpreter python-interpreter)

;; Increase gc-cons-threshold to improve performance
(setq gc-cons-threshold 10000000)

;; Increase the amount of data emacs can read from a process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; disable the title bar text
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Ask "y" or "n" instead of "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

(set-frame-font "Jetbrains Mono 16" nil t)
;; get rid of right fringe
(set-face-attribute 'fringe nil :background nil)

;; Change startup window size
(add-to-list 'default-frame-alist '(width . 100)) ; characters
(add-to-list 'default-frame-alist '(height . 45)) ; lines

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)

;; prevents line wrapping
(set-default 'truncate-lines t)

(save-place-mode 1)

(global-auto-revert-mode t)

(setq initial-major-mode 'fundamental-mode)

;; Avoid performance issues in files with very long lines.
(global-so-long-mode 1)

;; Turn off starting message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Remove all backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Enable line numbers and col numbers
(global-linum-mode +1)
(setq linum-format "%4d ")
(add-hook 'term-mode-hook (lambda () (linum-mode 0)))
(add-hook 'shell-mode-hook (lambda () (linum-mode 0)))
(setq column-number-mode t)

;; Isearch convenience, space matches anything (non-greedy)
(setq search-whitespace-regexp ".*?")

;; Disable interface
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
;; Remove initial scratch message
(setq initial-scratch-message "")

;; spaces not tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 200 4))

;; Set max columns to 80
(setq-default fill-column 80)

;; Disable alarm sound
(setq ring-bell-function 'ignore)

;; Deletes selected region
(delete-selection-mode 1)

;; Make all file names unique
(require 'uniquify)

;; Electric Pair mode
(electric-pair-mode 1)

;; fix mouse behavior
(setq mouse-yank-at-point t)

;; Make org mode source code syntax highlighted
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(add-hook 'org-mode-hook 'auto-fill-mode)

(global-set-key (kbd "C-u") 'undo)
(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "M-i") 'dabbrev-expand)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-*") 'universal-argument)
(global-set-key (kbd "S-<f13>") 'yank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM ELISP FUNCTIONS             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expose-global-binding-in-term (binding)
    (define-key term-raw-map binding
      (lookup-key (current-global-map) binding)))
(eval-after-load "ansi-term"
  '(expose-global-binding-in-term (kbd "M-x")))

(defun zs/term ()
  (interactive)
  (ansi-term (getenv "SHELL")))

(defun zs/save-position ()
  (interactive)
  (message "Saving position...")
  (set-register 49 (point-marker)))

(defun zs/goto-position ()
  (interactive)
  (jump-to-register 49))

(defun zs/delete-space-or-word ()
  (interactive)
  (cond
   ((= (point) (line-beginning-position))
    (delete-indentation))
   ((looking-back "\\s-\\s-+")
    (delete-horizontal-space))
   (t
    (backward-kill-word 1))
   ))

(global-set-key (kbd "C-<backspace>") 'zs/delete-space-or-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAC SPECIFIC SETTINGS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when is-mac
    (progn
      (setq mac-option-key-is-meta nil
            mac-command-key-is-meta t
            mac-command-modifier 'meta
            mac-option-modifier 'none
            ispell-program-name "aspell")
      (set-frame-font "Jetbrains Mono 15" nil t)
      (menu-bar-mode t)
      (global-set-key [mouse-2] 'yank)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WINDOWS (WSL) SPECIFIC SETTINGS    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up browser to open in windows
(when (string-match "-[Mm]icrosoft" operating-system-release)
  (setq
   browse-url-generic-program  "cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIRED                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key* "C-x C-j" 'dired-jump)

(with-eval-after-load 'dired

  ;; by default, omit hidden files
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  
  ;; rebind omit mode to something more useful
  (bind-key "M-o" 'dired-omit-mode 'dired-mode-map)
  (bind-key "DEL" 'dired-up-directory 'dired-mode-map)

  (when is-mac (setq insert-directory-program "gls"))

  ;; show directories first
  (setq dired-listing-switches "-alhF --group-directories-first")
  ;; ignore dotfiles by default
  (setq dired-omit-files "^\\\.\\|\\`[.]?#\\|\\`[.][.]?\\'"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :if is-mac
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :config
  (which-key-mode))

(use-package doom-modeline
  :if (display-graphic-p)
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-vcs-max-length 20)
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-checker-simple-format t))

(use-package expand-region
  :init
  (bind-key* "C-j" 'er/expand-region)
  :config
  (setq shift-select-mode nil)
  (setq expand-region-fast-keys-enabled nil))

(use-package doom-themes
  :if (display-graphic-p)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t))

;; if above themes not used
(unless (display-graphic-p)
    (load-theme 'wombat t))

(use-package counsel
  :demand
  :bind (("C-;" . counsel-imenu)
         ("C-c s" . swiper)
         ("C-c C-s" . swiper)
         ("C-c f" . counsel-rg)
         ("C-c o" . counsel-git)
         ("M-x" . counsel-M-x)
         ("s-x" . counsel-M-x)
         ("C-x b" . ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-c r" . counsel-buffer-or-recentf)
         ("C-c y" . counsel-yank-pop)
         (:map counsel-find-file-map
               ("RET" . ivy-alt-done)
               ("C-d" . ivy-done)))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-extra-directories nil)
  (setq ivy-sort-matches-functions-alist
        '((t)
          (counsel-find-file . ivy--sort-files-by-date))))

(use-package company
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort))
  :config
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf
           company-dabbrev)
          ))
  (setq company-idle-delay 0.1)
  (setq company-dabbrev-downcase nil))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind (("C-c C-h" . lsp-describe-thing-at-point))
  :hook ((rust-mode . lsp)
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config

  ;; make sure to "pip install future"
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)))

  (setq lsp-prefer-flymake nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-modeline-diagnostics-mode t)

  ;; pyls specific settings
  (setq lsp-pyls-plugins-pycodestyle-max-line-length 100)
  (setq lsp-pyls-plugins-flake8-max-line-length 100)
  )

(use-package lsp-ui :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable nil))

(use-package treemacs)

(use-package lsp-treemacs
  :bind ("C-c c l" . lsp-treemacs-errors-list))

(use-package flycheck
  :bind (("C-c c n" . flycheck-next-error)
         ("C-c c p" . flycheck-previous-error))
  :hook ((python-mode . flycheck-mode)
         (rust-mode . flycheck-mode)
         (json-mode . flycheck-mode)))

(use-package pyvenv)

(use-package blacken)

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy))

(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-c I" . crux-find-user-init-file)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c R" . crux-rename-file-and-buffer)
   ("C-c C" . crux-copy-file-preserve-attributes)
   ("C-k" . crux-smart-kill-line)))

(use-package undo-tree
  :bind (("C-u" . undo-tree-undo)
         ("C-x u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode))

(use-package all-the-icons-ivy
  :if (display-graphic-p)
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package smex
  :config
  (smex-initialize))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-items '((projects  . 5)
                        (recents . 5)
                        (agenda . 5))))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-no-confirm '(stage-all-changes unstage-all-changes)))

(use-package projectile
  :bind-keymap
  ("M-p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-sort-order 'recently-active)

  ;; set project search path based on which computer we are on
  (if is-personal-computer
      (setq projectile-project-search-path '("~/Documents/Programming/"))
    (setq projectile-project-search-path '("~/"))))

(use-package smart-jump
  :config
  (smart-jump-setup-default-registers))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'flyspell-mode))

(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode))

(use-package bm
  :bind (("C-c ," . bm-next)
         ("C-c ." . bm-toggle))
  :config
  (setq bm-highlight-style 'bm-highlight-only-fringe)
  (setq bm-in-lifo-order t))

(use-package groovy-mode)

(use-package vterm
  :init
  (defun vterm-copy-mode-enable ()
    (interactive)
    (vterm-copy-mode 1))
  (defun vterm-copy-mode-disable ()
    (interactive)
    (vterm-copy-mode 0))
  :bind (("M-t" . vterm)
         (:map vterm-mode-map
               ("C-c C-j" . vterm-copy-mode-enable)
               ("<S-insert>" . vterm-yank-primary)
               ("<S-f13>" . vterm-yank-primary))
         (:map vterm-copy-mode-map
               ("C-c C-k" . vterm-copy-mode-disable)
               ("RET" . vterm-copy-mode-disable)
               ("<return>" . vterm-copy-mode-disable)))
  :config
  (add-hook 'vterm-mode-hook (lambda () (linum-mode 0)))
  (setq vterm-shell shell-file-name)
  ;; used for shell pop
  (unbind-key "C-t" vterm-mode-map))

(use-package shell-pop
  :bind (("C-t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("vterm" "*shell-pop-term*" (lambda nil (vterm shell-pop-term-shell)))))
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  (setq shell-pop-term-shell shell-file-name))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

(use-package multiple-cursors)


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(load custom-file 'noerror)
(when is-personal-computer (load "~/.emacs.d/personal.el" 'noerror))
(unless is-personal-computer (load "~/.emacs.d/work.el" 'noerror))

;; start server
(server-start)
