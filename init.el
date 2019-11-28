;; Set up package management
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

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

(set-default-font "Courier 16" nil t)
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

(hl-line-mode 1)

;; Make all file names unique
(require 'uniquify)

;; Electric Pair mode
(electric-pair-mode 1)


(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

;; Make org mode source code syntax highlighted
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)

;; MODE LINE
;; (setq-default mode-line-format
;;               '("%e"
;;                 mode-line-front-space
;;                 mode-line-modified
;;                 " "
;;                 default-directory
;;                 mode-line-buffer-identification
;;                 " "
;;                 mode-line-position
;;                 (vc-mode vc-mode)
;;                 (flycheck-mode flycheck-mode-line)
;;                 " "
;;                 mode-line-misc-info
;;                 mode-line-end-spaces))


;; KEYBINDINGS
(global-set-key (kbd "C-u") 'undo)
(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "C-a") 'smart-line-beginning)
(global-set-key (kbd "s-w") 'kill-ring-save)
(global-set-key (kbd "s-i") 'dabbrev-expand)
(global-set-key (kbd "M-i") 'dabbrev-expand)
(global-set-key (kbd "s-f") 'forward-word)
(global-set-key (kbd "s-b") 'backward-word)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

;; PACKAGES

(use-package base16-theme
  :if window-system
  :config
  (load-theme 'base16-default-dark t))

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
  :init (define-key org-mode-map "\C-j" 'er/expand-region)
  :config
  (setq shift-select-mode nil))

(use-package counsel
  :demand
  :bind (("C-;" . counsel-imenu)
         ("C-c s" . swiper)
         ("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
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
  (setq company-idle-delay 0.2))

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

(use-package crux
  :bind
  (("C-c I" . crux-find-user-init-file)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c R" . crux-rename-file-and-buffer)
   ("C-c t" . crux-visit-term-buffer)
   ("C-c C" . crux-copy-file-preserve-attributes)))


(setq org-publish-project-alist
      '(("Shetlandic"
         :base-directory "~/Documents/Constructed/Shetlandic/"
         :publishing-directory "~/Documents/Constructed/Shetlandic/static"
         :publishing-function org-html-publish-to-html
         :exclude "proto.*")))


(use-package dash-functional)


(define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
  "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles,
made unique when necessary."
  :global t
  (if unpackaged/org-export-html-with-useful-ids-mode
      (progn
        (advice-add #'org-export-new-title-reference :override #'unpackaged/org-export-new-title-reference)
        (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference))
    (advice-remove #'org-export-new-title-reference #'unpackaged/org-export-new-title-reference)
    (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))

(defun unpackaged/org-export-get-reference (datum info)
  "Like `org-export-get-reference', except uses heading titles instead of random numbers."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq datum cache))
        (let* ((crossrefs (plist-get info :crossrefs))
               (cells (org-export-search-cells datum))
               ;; Preserve any pre-existing association between
               ;; a search cell and a reference, i.e., when some
               ;; previously published document referenced a location
               ;; within current file (see
               ;; `org-publish-resolve-external-link').
               ;;
               ;; However, there is no guarantee that search cells are
               ;; unique, e.g., there might be duplicate custom ID or
               ;; two headings with the same title in the file.
               ;;
               ;; As a consequence, before re-using any reference to
               ;; an element or object, we check that it doesn't refer
               ;; to a previous element or object.
               (new (or (cl-some
                         (lambda (cell)
                           (let ((stored (cdr (assoc cell crossrefs))))
                             (when stored
                               (let ((old (org-export-format-reference stored)))
                                 (and (not (assoc old cache)) stored)))))
                         cells)
                        (when (org-element-property :raw-value datum)
                          ;; Heading with a title
                          (unpackaged/org-export-new-title-reference datum cache))
                        ;; NOTE: This probably breaks some Org Export
                        ;; feature, but if it does what I need, fine.
                        (org-export-format-reference
                         (org-export-new-reference cache))))
               (reference-string new))
          ;; Cache contains both data already associated to
          ;; a reference and in-use internal references, so as to make
          ;; unique references.
          (dolist (cell cells) (push (cons cell new) cache))
          ;; Retain a direct association between reference string and
          ;; DATUM since (1) not every object or element can be given
          ;; a search cell (2) it permits quick lookup.
          (push (cons reference-string datum) cache)
          (plist-put info :internal-references cache)
          reference-string))))

(defun unpackaged/org-export-new-title-reference (datum cache)
  "Return new reference for DATUM that is unique in CACHE."
  (cl-macrolet ((inc-suffixf (place)
                             `(progn
                                (string-match (rx bos
                                                  (minimal-match (group (1+ anything)))
                                                  (optional "--" (group (1+ digit)))
                                                  eos)
                                              ,place)
                                ;; HACK: `s1' instead of a gensym.
                                (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                           (match-string 2 ,place)))
                                        (suffix (if suffix
                                                    (string-to-number suffix)
                                                  0)))
                                  (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
    (let* ((title (org-element-property :raw-value datum))
           (ref (url-hexify-string (substring-no-properties title)))
           (parent (org-element-property :parent datum)))
      (while (--any (equal ref (car it))
                    cache)
        ;; Title not unique: make it so.
        (if parent
            ;; Append ancestor title.
            (setf title (concat (org-element-property :raw-value parent)
                                "--" title)
                  ref (url-hexify-string (substring-no-properties title))
                  parent (org-element-property :parent parent))
          ;; No more ancestors: add and increment a number.
          (inc-suffixf ref)))
      ref)))

(setq ispell-program-name "aspell")
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

(add-hook 'org-mode-hook 'unpackaged/org-export-html-with-useful-ids-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(ansi-term-color-vector
   [unspecified "#f8f8f8" "#ab4642" "#a1b56c" "#f7ca88" "#7cafc2" "#ba8baf" "#7cafc2" "#383838"] t)
 '(company-box-enable-icon nil)
 '(custom-enabled-themes (quote (base16-default-dark)))
 '(custom-safe-themes
   (quote
    ("16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" default)))
 '(doom-modeline-mode nil)
 '(lsp-ui-sideline-enable nil)
 '(mood-line-mode t)
 '(org-cycle-hook
   (quote
    (org-cycle-hide-archived-subtrees org-cycle-show-empty-lines org-optimize-window-after-visibility-change)))
 '(package-selected-packages
   (quote
    (mood-line company-anaconda anaconda-mode dash-functional worf yasnippet flycheck-rust cargo dumb-jump rust-mode crux pipenv company-box company undo-tree smex which-key use-package ido-vertical-mode ido-completing-read+ flx-ido expand-region exec-path-from-shell esup counsel base16-theme)))
 '(unpackaged/org-export-html-with-useful-ids-mode t)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-candidate ((t (:foreground "white")))))
