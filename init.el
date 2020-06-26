;; Set up package management
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET DEFAULT CONFIGURATIONS         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set default python interpreter
(defconst python-interpreter "python3.7")

;; disable the title bar text
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Ask "y" or "n" instead of "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

(set-default-font "Jetbrains Mono 15" nil t)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM ELISP FUNCTIONS             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zs/term ()
  (interactive)
  (ansi-term (getenv "SHELL")))
(global-set-key (kbd "M-t") 'zs/term)

(defun zs/save-position ()
  (interactive)
  (message "Saving position...")
  (set-register 49 (point-marker)))

(defun zs/goto-position ()
  (interactive)
  (jump-to-register 49))

(global-set-key (kbd "C-c .") 'zs/save-position)
(global-set-key (kbd "C-c ,") 'zs/goto-position)

(defun zs/delete-indentation-or-word ()
  (interactive)
  (if (looking-back "^\\s-*")
      (delete-horizontal-space)
    (backward-kill-word 1)))

(global-set-key (kbd "C-<backspace>") 'zs/delete-indentation-or-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAC SPECIFIC (PERSONAL) SETTINGS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (eq system-type 'darwin)
    (progn
      (setq mac-option-key-is-meta nil
            mac-command-key-is-meta t
            mac-command-modifier 'meta
            mac-option-modifier 'none
            ispell-program-name "aspell")
      (load "~/.emacs.d/personal.el" 'noerror)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
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
  (setq doom-modeline-vcs-max-length 20))

(use-package expand-region
  :bind ("C-j" . er/expand-region)
  :config
  (setq shift-select-mode nil)
  (setq expand-region-fast-keys-enabled nil)
  (define-key org-mode-map "\C-j" 'er/expand-region))

(use-package doom-themes
  :if (display-graphic-p)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))

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
         ("C-c r" . counsel-recentf)
         (:map counsel-find-file-map
               ("RET" . ivy-alt-done)
               ("C-j" . ivy-immediate-done)))
  :config
  (ivy-mode 1)
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map "\C-s" 'swiper)))
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
  (setq company-idle-delay 0.2)
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
  (setq lsp-prefer-flymake nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-enable-snippet nil))

(use-package lsp-ui :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable nil))

(use-package company-lsp :commands company-lsp
  :config
  (setq company-lsp-enable-snippet nil))

(use-package flycheck
  :bind (("C-c C-n" . flycheck-next-error)
         ("C-c C-p" . flycheck-previous-error))
  :hook ((python-mode . flycheck-mode)
         (rust-mode . flycheck-mode)
         (json-mode . flycheck-mode))
  :config
  (setq flycheck-python-flake8-executable python-interpreter))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy))

(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-c I" . crux-find-user-init-file)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c R" . crux-rename-file-and-buffer)
   ("C-c C" . crux-copy-file-preserve-attributes)))

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

(use-package shell-pop
  :bind (("C-t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*shell-pop-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  (require 'term)
  (defun expose-global-binding-in-term (binding)
    (define-key term-raw-map binding
      (lookup-key (current-global-map) binding)))
  (expose-global-binding-in-term (kbd "C-t")))

(use-package magit)

(use-package projectile)

(use-package simpleclip
  :unless (memq window-system '(mac ns))
  :config
  (setq interprogram-cut-function 'simpleclip-set-contents)
  (simpleclip-mode 1)
  ;;; NOTE: PATCHED simpleclip-paste
  ;;; TO REMOVE * from interactive
  )

(use-package yasnippet)

(use-package smart-jump
  :bind (("M-?" . xref-find-references))
  :config
  (smart-jump-setup-default-registers)
  (global-set-key (kbd "M-?") 'xref-find-references))

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

(use-package worf
  :config
  ;; use ivy to insert a link to a heading in the current document
  ;; based on `worf-goto`
  (defun zs/worf-insert-internal-link ()
    "Use ivy to insert a link to a heading in the current `org-mode' document. Code is based on `worf-goto'."
    (interactive)
    (let ((cands (worf--goto-candidates)))
      (ivy-read "Heading: " cands
                :action 'zs/worf-insert-internal-link-action)))
  (defun zs/worf-insert-internal-link-action (x)
    "Insert link for `zs/worf-insert-internal-link'"
    ;; go to heading
    (save-excursion
      (goto-char (cdr x))
      ;; store link
      (call-interactively 'org-store-link)
      )
    ;; return to original point and insert link
    (org-insert-last-stored-link 1)
    ;; org-insert-last-stored-link adds a newline so delete this
    (delete-backward-char 1)
    )
  (define-key org-mode-map (kbd "C-c C-l") 'zs/worf-insert-internal-link))

(load custom-file 'noerror)

;; start server
(server-start)
