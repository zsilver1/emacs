(setq org-publish-project-alist
      '(
        ("Shetlandic"
         :base-directory "~/Documents/Constructed/Shetlandic/"
         :publishing-directory "~/Documents/Constructed/Shetlandic/static/"
         :publishing-function org-html-publish-to-html
         :exclude "proto.*")
        ))


(setq ispell-program-name "aspell")
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

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

(setq org-export-allow-bind-keywords t)

(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
