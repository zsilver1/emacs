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
;;(use-package solarized-theme)
(load-theme 'base16-default-dark t) ;; dark theme
;;(load-theme 'base16-default-light t) ;;light theme

;; disable the title bar text
(setq frame-title-format "")

;; Ask "y" or "n" instead of "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

(set-default-font "Courier 14" nil t)

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
  (global-set-key (kbd "C-;") 'counsel-imenu)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "s-g") 'counsel-git-grep)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-extra-directories nil))

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
 '(package-selected-packages
   (quote
    (merlin tuareg which-key use-package ido-vertical-mode ido-completing-read+ flx-ido expand-region exec-path-from-shell esup counsel base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
