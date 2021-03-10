;; VARIABLES
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)

(setq org-tags-column -70)
(setq org-fast-tag-selection-single-key 'expert)
(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(c@)")))

(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;; HOOKS
(add-hook 'org-mode-hook 'auto-fill-mode)

;; KEYBINDINGS
(global-set-key (kbd "C-c a") (lambda () (interactive) (org-agenda nil "t")))

(use-package org-super-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Next"
                 :order 1
                 :todo "NEXT"
                 :date today
                 :scheduled today)
          (:name "Prioritized"
                 :and (:priority "A" :todo "TODO"))
          (:name "Upcoming"
                 :auto-planning)
          (:name "Waiting"
                 :todo "WAITING")
          (:name "Tasks"
                 :category "tasks")
          (:name "Projects"
                 :auto-outline-path))))
