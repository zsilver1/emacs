;; enable xml mode for schemas
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . xml-mode))


(defun rsync-project ()
  (interactive)
  (require 'projectile)
  (with-output-to-temp-buffer "*shell-command-output*"
    (shell-command (concat "source " (getenv "HOME") "/.zshrc && devrsync " (projectile-project-root))
                   "*shell-command-output*"
                   "*shell-command-output*")
    (pop-to-buffer "*shell-command-output*")
    (help-mode)))
