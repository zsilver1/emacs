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
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Ask "y" or "n" instead of "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

(set-default-font "Courier 15" nil t)

;; Change startup window size
(add-to-list 'default-frame-alist '(width . 100)) ; characters
(add-to-list 'default-frame-alist '(height . 45)) ; lines

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)

(save-place-mode 1)

(global-auto-revert-mode t)

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

;; Electric Pair mode
(electric-pair-mode 1)

(setq ido-everywhere t)
(ido-mode 1)
(setq ido-ignore-extensions t)

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

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

(defun update-org-calendar ()
  "Syncs emacs with icloud calendar."
  (interactive)
  (call-process "~/.emacs.d/cloudsync.sh"))

(add-hook 'before-init-hook 'update-org-calendar)

;; ORG MODE
;; Make org mode source code syntax highlighted
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-agenda-files (quote ("~/Dropbox/org"
                               "~/.emacs.d/calendar.org")))
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/inbox.org")

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold))))


(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-'") 'org-cycle-agenda-files)

(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
	     "* Note %?\n%T")
	    ("l" "Link" entry (file+headline "~/Dropbox/org/notes.org" "Links")
	     "* %? %^L %^g \n%T" :prepend t)
	    ("t" "To Do Item" entry (file+headline "~/Dropbox/org/tasks.org" "To Do Items")
	     "* %?\n%T" :prepend t)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-j") 'er/expand-region))

(setq org-agenda-timegrid-use-ampm t)

;; for startup
(setq inhibit-splash-screen t)
(org-agenda-list)
(delete-other-windows)

;; KEYBINDINGS
;; (setq mac-command-key-is-meta t)
;; (setq mac-command-modifier 'meta)
(global-set-key (kbd "C-u") 'undo)
(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-a") 'smart-line-beginning)
(global-set-key (kbd "s-w") 'kill-ring-save)
(global-set-key (kbd "s-i") 'hippie-expand)
(global-set-key (kbd "M-i") 'hippie-expand)
(global-set-key (kbd "s-f") 'forward-word)
(global-set-key (kbd "s-b") 'backward-word)

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
  (global-set-key (kbd "C-c s") 'swiper)
  (global-set-key (kbd "C-c f") 'counsel-rg)
  (global-set-key (kbd "s-o") 'counsel-git)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "s-x") 'counsel-M-x)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-extra-directories nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dashboard counsel-etags undo-tree smex which-key use-package ido-vertical-mode ido-completing-read+ flx-ido expand-region exec-path-from-shell esup counsel base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
