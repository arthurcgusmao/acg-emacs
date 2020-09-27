;; Rainbow identifiers colors syntax elements of the code in unique colors
(use-package rainbow-identifiers)

;; General bibliography handling (bulit-in package)
(use-package reftex
  :straight nil
  :config
  (add-to-list 'reftex-default-bibliography acg/default-bib-file)

  (defun acg/reftex-add-to-default-bibliography ()
    "Prompts the user for a filepath to add to the default
bibliography file."
    (interactive)
    (let ((file (read-file-name "Select bib file:")))
      (add-to-list 'reftex-default-bibliography file))))
