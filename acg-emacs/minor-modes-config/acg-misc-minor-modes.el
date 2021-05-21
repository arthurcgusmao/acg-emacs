;; Rainbow identifiers colors syntax elements of the code in unique colors
(use-package rainbow-identifiers)

;; Pandoc integration with Emacs
(use-package pandoc-mode)


;; General bibliography handling (bulit-in packages)
(use-package reftex
  :straight nil
  :config
  (add-to-list 'reftex-default-bibliography acg/default-bib-file))

(use-package bibtex
  :straight nil
  :bind
  (:map bibtex-mode-map
        ("C-j" . nil)))

(use-package ivy-bibtex
  :after ivy bibtex
  :config
  (add-to-list 'bibtex-completion-bibliography acg/default-bib-file))

;; org-ref package configurations
(use-package org-ref
  :after org
  :config
  (add-to-list 'org-ref-default-bibliography acg/default-bib-file)

  ;; @todo: See org-ref for use of these variables
  ;; (setq org-ref-bibliography-notes (concat user-emacs-directory "bibliography/notes.org")
  ;; org-ref-pdf-directory (concat user-emacs-directory "bibliography/bibtex-pdfs/"))
  :bind
  (:map org-ref-cite-keymap
        ("<S-up>" . nil)                ; Previously org-ref-sort-citation-link
        ("<S-right>" . nil) ; Previously (lambda nil (interactive) (org-ref-swap-citation-link 1)))
        ("<S-left>" . nil) ; Previously (lambda nil (interactive) (org-ref-swap-citation-link -1)))
        ("<C-right>" . nil)
        ("<C-left>" . nil)
        ("<C-M-right>" . org-ref-next-key)
        ("<C-M-left>" . org-ref-previous-key)))

(defun acg/reftex-bibtex-add-to-default-bibliography ()
  "Prompts the user for a filepath to add to the default
bibliography file variables."
  (interactive)
  (let ((file (expand-file-name
               (read-file-name "Select bib file:"))))
    (add-to-list 'reftex-default-bibliography file)
    (add-to-list 'bibtex-completion-bibliography file)
    (add-to-list 'org-ref-bibliography-files file)))
