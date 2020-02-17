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

(add-hook 'org-mode-hook 'unpackaged/org-export-html-with-useful-ids-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
