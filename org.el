;; VARIABLES
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)

(setq org-tags-column -70)
(setq org-fast-tag-selection-single-key 'expert)
(setq org-tag-alist '(("today" . ?t)))
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(c@)")))


(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-agenda-filename "Tasks")
         "* TODO %?\n  %i")))


(setq org-agenda-files `(,org-agenda-filename))
(setq org-directory (file-name-directory org-agenda-filename))

;; HOOKS
(add-hook 'org-mode-hook 'auto-fill-mode)

;; KEYBINDINGS
(global-set-key (kbd "C-c a") (lambda () (interactive) (org-agenda nil "t")))

(use-package org-super-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Today"
                 :tag "today")
          (:name "High Priority"
                 :priority "A")
          (:name "Upcoming"
                 :auto-planning)
          (:auto-parent))))
