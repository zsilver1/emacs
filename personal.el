(use-package org-journal
  :custom
  (org-journal-dir "~/Dropbox/Journal/")
  (org-journal-file-type "monthly")
  (org-journal-file-format "%Y/%Y-%m.org")
  (org-journal-find-file 'find-file)
  (org-journal-time-format "%l:%M %p")
  (org-journal-time-prefix "**")
  (org-journal-search-results-order-by :desc)
  :init
  (defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
      (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
      (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
      (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

(setq org-journal-file-header 'org-journal-file-header-func))


(require 'org-table)

(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))
    ))

(add-hook 'yaml-mode-hook 'orgtbl-mode)
(add-hook 'yaml-mode-hook
          (lambda()
            (add-hook 'after-save-hook 'cleanup-org-tables  nil 'local)))
