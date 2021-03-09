;; VARIABLES
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)

(setq org-tags-column -70)
(setq org-fast-tag-selection-single-key 'expert)
(setq org-tag-alist '(("today" . ?t)))
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(c@)")))

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
        '((:name "Today"
                 :tag "today"
                 :date today
                 :scheduled today)
          (:name "Prioritized"
                 :priority>= "C")
          (:name "Upcoming"
                 :auto-planning)
          (:auto-category))))
