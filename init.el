;; Set up package management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)


;; Set up use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(use-package base16-theme)
(load-theme 'base16-default-dark t) ;; dark theme
;;(load-theme 'base16-default-light t) ;;light theme

;; disable the title bar text
(setq frame-title-format "")

;; Ask "y" or "n" instead of "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

(set-default-font "Courier 15" nil t)

;; Change startup window size
(add-to-list 'default-frame-alist '(width . 100)) ; characters
(add-to-list 'default-frame-alist '(height . 45)) ; lines

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)

;; Turn off starting message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Remove all backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Enable line numbers and col numbers
(global-linum-mode +1)
(setq linum-format "%3d \u2502")
(setq column-number-mode t)

;; Isearch convenience, space matches anything (non-greedy)
(setq search-whitespace-regexp ".*?")

;; Disable interface
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;; Remove initial scratch message
(setq initial-scratch-message "")

;; spaces not tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 200 4))

;; Set max columns to 80
(setq-default fill-column 80)

;; Change startup window size

;; Enable windmove mode
(windmove-default-keybindings)

;; Disable alarm sound
(setq ring-bell-function 'ignore)

;; Move minimum when cursor exits view, instead of recentering
(setq scroll-conservatively 101)

;; Mouse scroll moves 1 line at a time, instead of 5 lines
(setq mouse-wheel-scroll-amount '(1))

;; On a long mouse scroll keep scrolling by 1 line
(setq mouse-wheel-progressive-speed nil)

;; Deletes selected region
(delete-selection-mode 1)

;; Make all file names unique
(require 'uniquify)

;; Save your place in buffers
(require 'saveplace)
(setq-default save-place t)

;; Electric Pair mode
(electric-pair-mode 1)

(setq ido-everywhere t)
(ido-mode 1)
(setq ido-ignore-extensions t)

(setq ispell-program-name "aspell")

(setq-default mode-line-format
              '(" "
                
                mode-line-modified
                " "
                mode-line-buffer-identification
                "   (%l:%c)   "
                vc-mode
                "   ("
                mode-name")   "
                flycheck-mode-line
                mode-line-end-spaces))




;; PACKAGES

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package smex)

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package flx-ido
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  (setq flx-ido-threshold 10000))


(use-package which-key
  :config
  (which-key-mode))

(use-package expand-region
  :config
  (global-set-key (kbd "C-j") 'er/expand-region)
  (setq shift-select-mode nil))

(use-package counsel
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (global-set-key (kbd "C-;") 'counsel-imenu)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "s-g") 'counsel-ag)
  (global-set-key (kbd "s-o") 'counsel-git)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-extra-directories nil))

(use-package elpy
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-python-command "python3"))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf
           company-dabbrev-code
           company-abbrev company-dabbrev
           )))
  (setq company-idle-delay 1)
  (setq company-minimum-prefix-length 1)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

(use-package flycheck
  :init
  (setq flycheck-enabled-mode-hooks '(
                                      c-mode-hook
                                      c++-mode-hook
                                      python-mode-hook
                                      rust-mode-hook
                                      ))
  :config
  (mapc (lambda (hook)
          (add-hook hook 'flycheck-mode))
        flycheck-enabled-mode-hooks)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14"))))

(use-package rust-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (setq rust-format-on-save t))

(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; ORG MODE
;; Make org mode source code syntax highlighted
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)

;; KEYBINDINGS
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(flycheck-python-flake8-executable "/usr/local/bin/flake8")
 '(package-selected-packages
   (quote
    (undo-tree racer flycheck-rust rust-mode smex flycheck elpy which-key use-package ido-vertical-mode ido-completing-read+ flx-ido expand-region exec-path-from-shell esup counsel base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:underline "#ab4642"))))
 '(flycheck-info ((t (:underline "#a1b56c"))))
 '(flycheck-warning ((t (:underline "#dc9656")))))
