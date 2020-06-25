;; Set up package management
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")

(server-start)
(require 'org)
;; Set up use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;; disable the title bar text
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Ask "y" or "n" instead of "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

(set-default-font "Consolas 19" nil t)
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
; (setq linum-format "%1d \u2502")
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

;; Make org mode source code syntax highlighted
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)

;; KEYBINDINGS
(global-set-key (kbd "C-u") 'undo)
(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "M-i") 'dabbrev-expand)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

(defun zs/term ()
  (interactive)
  (ansi-term (getenv "SHELL")))
(global-set-key (kbd "M-t") 'zs/term)


(if (eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier 'none))


;; PACKAGES
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :config
  (which-key-mode))

(use-package mood-line
  :config
  (mood-line-mode))

(use-package expand-region
  :bind ("C-j" . er/expand-region)
  :config
  (setq shift-select-mode nil)
  (define-key org-mode-map "\C-j" 'er/expand-region)
  (define-key latex-mode-map "\C-j" 'er/expand-region))

(use-package doom-themes
  :if (display-graphic-p)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one-light t))

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
  )

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :bind (("M-." . lsp-find-definition)
         ("M-?" . lsp-find-references))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-signature-render-documentation nil))

(use-package lsp-ui :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable nil))

(use-package company-lsp :commands company-lsp)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package flycheck
  :hook ((python-mode . flycheck-mode)
         (rust-mode . flycheck-mode)))

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

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-enable-icon nil))

(use-package all-the-icons-ivy
  :if (display-graphic-p)
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer))

(defun zs/tex-compile-and-open ()
  (interactive)
  (let ((buf (buffer-file-name))
        (pdf (file-name-sans-extension (buffer-file-name))))
    (call-process-shell-command (concat "xelatex " buf "; open " pdf ".pdf"))))

(eval-after-load 'latex
  '(define-key LaTeX-mode-map (kbd "C-c C-e") zs/tex-compile-and-open))


(load custom-file 'noerror)
(load "personal.el")
