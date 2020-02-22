(use-package org)
(use-package crux)

;; configurations
(setq org-startup-with-inline-images t)
(setq org-image-actual-width '(400))
(setq org-startup-indented nil)
(setq org-adapt-indentation nil)
(setq org-startup-truncated nil)
(setq org-startup-folded nil)

;; make code blocks pretty
(setq org-src-fontify-natively t)
;; highlight latex related syntax
(eval-after-load 'org
  '(setf org-highlight-latex-and-related '(latex script entities)))


;; where to put latex preview images
(setq org-latex-preview-ltxpng-directory (concat user-emacs-directory "latex-png-previews/"))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

;; configuring the latex-pdf generator
(setq org-latex-pdf-process
      '("pdflatex %f" "bibtex %b" "pdflatex %f" "pdflatex %f"))


;; org-ref package configurations
(use-package org-ref)
(setq reftex-default-bibliography acg/default-bib-file
      org-ref-default-bibliography acg/default-bib-file)

;; see org-ref for use of these variables
;; (setq org-ref-bibliography-notes (concat user-emacs-directory "bibliography/notes.org")
;; org-ref-pdf-directory (concat user-emacs-directory "bibliography/bibtex-pdfs/"))

(use-package org-ref)


;; setting up keybindings
(add-hook 'org-mode-hook 
          (lambda ()
            (define-key org-mode-map (kbd "<C-tab>") nil)
            (define-key org-mode-map (kbd "<S-iso-lefttab>") 'org-cycle)
            (define-key org-mode-map (kbd "C-a") nil)
            (define-key org-mode-map (kbd "C-e") nil)
            (define-key org-mode-map (kbd "M-e") nil)
            (define-key org-mode-map (kbd "C-j") nil)
            (define-key org-mode-map (kbd "C-k") nil)
            (define-key org-mode-map (kbd "C-y") nil)
            (define-key org-mode-map (kbd "C-t") nil)
            
            (define-key org-mode-map (kbd "<S-right>") nil)
            (define-key org-mode-map (kbd "<S-left>") nil)
            (define-key org-mode-map (kbd "<S-up>") nil)
            (define-key org-mode-map (kbd "<S-down>") nil)
            (define-key org-mode-map (kbd "<C-S-right>") nil)
            (define-key org-mode-map (kbd "<C-S-left>") nil)
            (define-key org-mode-map [remap backward-paragraph] nil)
            (define-key org-mode-map [remap forward-paragraph] nil)
            (define-key org-mode-map (kbd "<C-S-up>") nil)
            (define-key org-mode-map (kbd "<C-S-down>") nil)
            (define-key org-mode-map (kbd "<M-S-end>") 'show-subtree)
            (define-key org-mode-map (kbd "<M-S-home>") 'hide-subtree)
            (define-key org-mode-map (kbd "<M-end>") 'org-cycle)
            (define-key org-mode-map (kbd "<M-home>") 'org-cycle-backwards)
            (define-key org-mode-map (kbd "<C-M-end>") 'org-global-cycle)
            (define-key org-mode-map (kbd "<C-M-home>") 'acg/org-global-cycle-backwards)
            (define-key org-mode-map (kbd "C-<") 'org-shiftmetaleft)
            (define-key org-mode-map (kbd "C->") 'org-shiftmetaright)

            (define-key org-mode-map (kbd "<M-up>") nil)
            (define-key org-mode-map (kbd "<M-down>") nil)

            (define-key org-mode-map (kbd "<return>") 'org-return-indent)
            (define-key org-mode-map (kbd "<C-return>") nil)
            (define-key org-mode-map (kbd "<C-S-return>") nil)
            (define-key org-mode-map (kbd "<M-return>") 'acg/org-meta-return-newline)
            (define-key org-mode-map (kbd "C-8") 'org-insert-heading-after-current)
            (define-key org-mode-map (kbd "M-8") 'org-insert-subheading-newline)
            
            (define-key org-mode-map (kbd "<M-S-right>") nil)
            (define-key org-mode-map (kbd "<M-S-left>") nil)
            
            ;; (acg/local-set-minor-mode-key 'smartparens-mode-map (kbd "<M-left>") 'org-cycle-backwards)
            ;; (acg/local-set-minor-mode-key 'smartparens-mode-map (kbd "<M-right>") 'org-cycle)
            (define-key org-mode-map (kbd "<M-left>") nil)
            (define-key org-mode-map (kbd "<M-right>") nil)

            (define-key org-mode-map (kbd "<f5>") (lambda () (interactive) (org-preview-latex-fragment 16)))
            (define-key org-mode-map (kbd "<f6>") 'org-preview-latex-fragment)
            (define-key org-mode-map (kbd "<f9>") 'org-latex-export-to-pdf)
            ))


;; custom function

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

(defun acg/org-global-cycle-backwards (&optional ARG)
  "Same as `org-global-cycle' but backwards."
  (interactive)
  (org-global-cycle ARG)
  (org-global-cycle ARG))

(defun org-insert-subheading-newline ()
  "Same as `org-insert-subheading' but creates a new line with blank subheading."
  (interactive)
  (org-insert-heading-after-current)
  (org-shiftmetaright))

(defun acg/org-meta-return-newline ()
  "Same as `org-meta-return' but creates a new line."
  (interactive)
  (org-forward-paragraph)
  (org-back-over-empty-lines)
  (previous-line)
  (end-of-line)
  (org-meta-return))


;; making org insert blank lines before headings
;; for more, see:
;; - https://www.reddit.com/r/emacs/comments/3pw2qq/orgmode_headings_and_blank_lines/
;; - https://emacs.stackexchange.com/questions/13311/make-org-blank-before-new-entry-distinguish-between-a-todo-list-and-a-text-outli

;; (setq org-blank-before-new-entry
;;       '((heading . always)
;;         (plain-list-item . nil)))

;; (defun call-rebinding-org-blank-behaviour (fn)
;;   (let ((org-blank-before-new-entry
;;          (copy-tree org-blank-before-new-entry)))
;;     (when (org-at-heading-p)
;;       (rplacd (assoc 'heading org-blank-before-new-entry) nil))
;;     (call-interactively fn)))

;; (defun smart-org-meta-return-dwim ()
;;   (interactive)
;;   (call-rebinding-org-blank-behaviour 'org-meta-return))

;; (defun smart-org-insert-todo-heading-dwim ()
;;   (interactive)
;;   (call-rebinding-org-blank-behaviour 'org-insert-todo-heading))

;; (defun smart-org-insert-heading-respent-content-dwim ()
;;   (interactive)
;;   (call-rebinding-org-blank-behaviour 'org-insert-heading-respect-content))

;; (defun smart-org-insert-todo-heading-respect-content-dwim ()
;;   (interactive)
;;   (call-rebinding-org-blank-behaviour 'org-insert-todo-heading-respect-content))

;; (define-key org-mode-map (kbd "M-<return>") 'smart-org-meta-return-dwim) 
;; (define-key org-mode-map (kbd "M-S-<return>") 'smart-org-insert-todo-heading-dwim) 
;; (define-key org-mode-map (kbd "C-<return>") 'smart-org-insert-heading-respent-content-dwim)
;; (define-key org-mode-map (kbd "C-S-<return>") 'smart-org-insert-todo-heading-respect-content-dwim)


;; crazyly trying to fix the subtree moving behavior

(defun acg/org-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  (setq arg (prefix-numeric-value arg))
  (let ((movfunc (if (> arg 0) 'org-get-next-sibling
		   'org-get-last-sibling))
	(ins-point (make-marker))
	(cnt (abs arg))
	(col (current-column))
	beg beg0 end txt folded ne-beg ne-end ne-ins ins-end)
    ;; Select the tree
    (org-back-to-heading)
    (setq beg0 (point)) ;; beg0 = point of heading start
    (save-excursion
      (setq ne-beg (org-back-over-empty-lines)) ;; ne-beg = number of empty lines above subtree to be moved
      (setq beg (point))) ;; beg = point considering empty lines above subtree
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (outline-invisible-p))) ;; var folded: if "subtree is folded" = outline, else nil
      (progn (org-end-of-subtree nil t)
	     (unless (eobp) (backward-char))))
    (outline-next-heading)
    (setq ne-end (org-back-over-empty-lines)) ;; ne-end = number of empty lines below subtree to be moved
    (setq end (point)) ;; end = point right after subtree (disconsidering whitespace below)
    (goto-char beg0)
    (when (and (> arg 0) (org-first-sibling-p) (< ne-end ne-beg)) ;; when is moving down, is first subtree of level and (< ne-end ne-beg)
      ;; include less whitespace
      (save-excursion
	(goto-char beg)
	(forward-line (- ne-beg ne-end))
	(setq beg (point))))
    ;; Find insertion point, with error handling
    (while (> cnt 0)
      (or (and (funcall movfunc) (looking-at org-outline-regexp))
	  (progn (goto-char beg0)
		 (user-error "Cannot move past superior level or buffer limit")))
      (setq cnt (1- cnt)))
    (if (> arg 0)
	;; Moving forward - still need to move over subtree
	(progn (org-end-of-subtree t t)
	       (save-excursion
		 (org-back-over-empty-lines)
		 (or (bolp) (newline)))))
    (setq ne-ins (org-back-over-empty-lines))
    (move-marker ins-point (point))
    (setq txt (buffer-substring beg end))
    (org-save-markers-in-region beg end)
    (delete-region beg end)
    (org-remove-empty-overlays-at beg)
    (or (= beg (point-min)) (outline-flag-region (1- beg) beg nil))
    (or (bobp) (outline-flag-region (1- (point)) (point) nil))
    (and (not (bolp)) (looking-at "\n") (forward-char 1))
    (let ((bbb (point)))
      (insert-before-markers txt)
      (org-reinstall-markers-in-region bbb)
      (move-marker ins-point bbb))
    (or (bolp) (insert "\n"))
    (setq ins-end (point))
    (goto-char ins-point)
    (org-skip-whitespace)
    (when (and (< arg 0)
	       (org-first-sibling-p)
	       (> ne-ins ne-beg))
      ;; Move whitespace back to beginning
      (save-excursion
        (goto-char ins-end)
        (let ((kill-whole-line t))
          (kill-line (- ne-ins ne-beg)) (point)))
      (insert (make-string (- ne-ins ne-beg) ?\n)))
    (move-marker ins-point nil)
    (if folded
	(hide-subtree)
      (org-show-entry)
      (show-children)
      (org-cycle-hide-drawers 'children))
    (org-clean-visibility-after-subtree-move)
    ;; move back to the initial column we were at
    (move-to-column col)))
