(require 'org)
(require 'crux)

;; configurations
(setq org-startup-with-inline-images t)
(setq org-startup-indented t)

;; where to put latex preview images
(setq org-latex-preview-ltxpng-directory "~/.emacs.d/latex-png-previews/")
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

(add-hook 'org-mode-hook 
          (lambda ()
            (define-key org-mode-map (kbd "<C-tab>") nil)
            (define-key org-mode-map (kbd "<S-iso-lefttab>") 'org-cycle)
            (define-key org-mode-map (kbd "C-a") nil)
            (define-key org-mode-map (kbd "C-e") nil)
            (define-key org-mode-map (kbd "C-j") nil)
            (define-key org-mode-map (kbd "C-k") nil)
            (define-key org-mode-map (kbd "C-y") nil)
            (define-key org-mode-map (kbd "C-t") nil)
            
            (define-key org-mode-map (kbd "<S-right>") nil)
            (define-key org-mode-map (kbd "<S-left>") nil)
            (define-key org-mode-map (kbd "<C-S-right>") nil)
            (define-key org-mode-map (kbd "<C-S-left>") nil)
            (define-key org-mode-map (kbd "<M-end>") 'show-subtree)
            (define-key org-mode-map (kbd "<M-home>") 'hide-subtree)
            (define-key org-mode-map (kbd "C-<") 'org-shiftmetaleft)
            (define-key org-mode-map (kbd "C->") 'org-shiftmetaright)

            (define-key org-mode-map (kbd "<return>") 'org-return-indent)
            (define-key org-mode-map (kbd "<C-return>") nil)
            (define-key org-mode-map (kbd "<C-S-return>") nil)
            (define-key org-mode-map (kbd "<M-return>") 'org-meta-return-newline)
            (define-key org-mode-map (kbd "C-8") 'org-insert-heading-after-current)
            (define-key org-mode-map (kbd "M-8") 'org-insert-subheading-newline)
            
            ;; (define-key org-mode-map (kbd "<M-S-right>") nil)
            ;; (define-key org-mode-map (kbd "<M-S-left>") nil)
            (acg-local-set-minor-mode-key 'smartparens-mode-map (kbd "<M-left>") 'org-cycle-backwards)
            (acg-local-set-minor-mode-key 'smartparens-mode-map (kbd "<M-right>") 'org-cycle)
            (define-key org-mode-map (kbd "<M-left>") 'org-cycle-backwards)
            (define-key org-mode-map (kbd "<M-right>") 'org-cycle)
            ))

;; (defun org-insert-heading-respect-content-above (&optional INVISIBLE-OK)
;;   "Same as `org-insert-heading-respect-content-above' but inserts on line above."
;;   (interactive)
;;   (crux-smart-open-line-above)
;;   (org-insert-heading-respect-content INVISIBLE-OK))

(defun org-cycle-backwards (&optional ARG)
  "Same as `org-cycle' but backwards."
  (interactive)
  (org-cycle ARG)
  (org-cycle ARG))

(defun org-insert-subheading-newline ()
  "Same as `org-insert-subheading' but creates a new line with blank subheading."
  (interactive)
  (org-insert-heading-after-current)
  (org-shiftmetaright))

(defun org-meta-return-newline ()
  "Same as `org-meta-return' but creates a new line."
  (interactive)
  (end-of-line)
  (org-meta-return))
