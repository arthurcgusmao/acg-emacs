(use-package org
  :config
  ;; configurations
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width '(400))
  (setq org-startup-indented nil)
  (setq org-adapt-indentation nil)
  (setq org-startup-truncated nil)
  (setq org-startup-folded nil)
  (setq org-list-allow-alphabetical t)

  ;; Export both source code and its results by default
  (setq org-babel-default-header-args
        (cons '(:exports . "both")
              (assq-delete-all :exports org-babel-default-header-args)))
  :bind
  (:map org-mode-map
        ("<C-down>" . org-insert-link)
        ("<C-tab>" . nil)
        ("<S-iso-lefttab>" . org-cycle)
        ("M-a" . nil)
        ("M-e" . nil)
        ("C-e" . org-edit-special)
        ("M-j" . nil)
        ("M-k" . org-insert-link)
        ("M-y" . nil)
        ("M-t" . nil)

        ("<S-left>" . nil)
        ("<S-right>" . nil)
        ("<S-up>" . nil)
        ("<S-down>" . nil)
        ("<M-left>" . nil)
        ("<M-right>" . nil)
        ("<C-left>" . nil)
        ("<C-right>" . nil)
        ("<M-up>" . nil)
        ("<M-down>" . nil)
        ("<C-up>" . org-backward-paragraph)
        ("<C-down>" . org-forward-paragraph)
        ("<C-s-up>" . acg/org-metaup)
        ("<C-s-down>" . acg/org-metadown)
        ;; (acg/local-set-minor-mode-key 'smartparens-mode-map (kbd "<C-left>") 'org-cycle-backwards)
        ;; (acg/local-set-minor-mode-key 'smartparens-mode-map (kbd "<C-right>") 'org-cycle)

        ("<M-S-right>" . nil)
        ("<M-S-left>" . nil)
        ("<M-S-up>" . nil)
        ("<M-S-down>" . nil)
        ("<C-S-end>" . show-subtree)
        ("<C-S-home>" . hide-subtree)
        ("<C-end>" . org-cycle)
        ("<home>" . acg/org-beginning-of-visual-line-or-indentation)
        ("<C-home>" . org-cycle-backwards)
        ("<C-M-end>" . org-global-cycle)
        ("<C-M-home>" . acg/org-global-cycle-backwards)
        ("M-<" . org-shiftmetaleft)
        ("M->" . org-shiftmetaright)

        ("RET" . acg/org-return)
        ("<M-return>" . acg/org-smart-open-line-below)
        ("<M-S-return>" . acg/org-smart-open-line-above)
        ("<C-RET>" . acg/org-open-line)
        ("<S-return>" . acg/org-newline-above)
        ;; ("<M-RET>" . acg/org-meta-return-newline)
        ("M-8" . org-insert-heading-after-current)
        ("C-8" . org-insert-subheading-newline)

        ("<C-S-left>" . nil)
        ("<C-S-up>" . nil)
        ("<C-S-down>" . nil)


        ("<f5>" . (lambda () (interactive) (org-preview-latex-fragment 16)))
        ("<f6>" . org-preview-latex-fragment)
        ("<f9>" . org-latex-export-to-pdf)

        ;; Org src map
        :map org-src-mode-map
        ("C-e" . org-edit-src-exit)
        ("M-s" . acg/org-edit-src-save-exit)
        ("M-w" . acg/org-edit-src-confirm-abort)
        )
  )

;;; Fonts and Appearence

;; make code blocks pretty
(setq org-src-fontify-natively t)
(setq org-fontify-quote-and-verse-blocks t)
;; hide markup (emphasis, italic, bold)
(setq org-hide-emphasis-markers t)
;; highlight latex related syntax
(eval-after-load 'org
  '(setf org-highlight-latex-and-related '(latex script entities)))

;; Quickfix: disable `org-latex-and-related' foreground to fix some SRC
;; metalines showing text in a different color after the underscore, and to
;; also adapt it to `org-export-with-sub-superscripts' only working with {}, as
;; configured below.
(set-face-attribute 'org-latex-and-related nil
                    :foreground nil)
;; Make Org export subscripts/superscripts only when part after _ or ^ is enclosed by {}
(setq org-export-with-sub-superscripts (make-symbol "{}"))


;; where to put latex preview images
(setq org-preview-latex-image-directory (concat temporary-file-directory "org-mode-ltximg-preview/"))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

;; configuring the latex-pdf generator
(setq org-latex-pdf-process
      '("pdflatex %f" "bibtex %b" "pdflatex %f" "pdflatex %f"))

;; Always redisplay inline images after executing and SRC block
(eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))


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

(defun acg/soft-kill-line ()
  "Same as `kill-line', but doesn't kill through newline."
  (interactive)
  (kill-region (point) (line-end-position)))

(defun acg/beginning-of-line-or-first-alphabetical-letter ()
  "Toggles cursor between beginning of line (column 0) and the
first alphabetical letter (a-zA-Z)."
  (interactive)
  (let ((p1 (point))
        (p2 (save-excursion (beginning-of-line) (skip-chars-forward "^a-zA-Z\n") (point))))
    (message "%s %s" p1 p2)
    (if (eq p1 p2)
        (beginning-of-line)
      (goto-char p2))))

(defun acg/org-beginning-of-visual-line-or-indentation (&optional arg)
  "Org alternative for
`acg/beginning-of-visual-line-or-indentation' that considers
being inside a list and toggles cursor between first alphatical
word in list (usually first word) and beginning of line."
  (interactive "^p")
  (if (org-in-item-p)
      (acg/beginning-of-line-or-first-alphabetical-letter)
    (acg/beginning-of-visual-line-or-indentation arg)))

;; (setq org-list-end-re "^[ \t]*\n")
(setq org-list-end-re "[ \t]*$")

(defun acg/org-smart-open-line-below ()
  "`acg/smart-open-line-below' version for org-mode. Performs
additional utilities like adding a bullet point if in a list or
wraping region if on a table."
  (interactive)
  (org-check-before-invisible-edit 'insert)
  (cond ((org-at-table-p) (call-interactively #'org-table-wrap-region))
	((org-in-item-p) (beginning-of-line) (org-insert-item) (org-metadown) (end-of-line))
	(t (call-interactively #'acg/smart-open-line-below))))

(defun acg/org-smart-open-line-above ()
  "`acg/smart-open-line-above' version for org-mode. Performs
additional utilities like adding a bullet point if in a list or
wraping region if on a table."
  (interactive)
  (org-check-before-invisible-edit 'insert)
  (cond ((org-at-table-p) (call-interactively #'org-table-wrap-region))
        ;; For bullet items, we wanna check if we're either ON a list or right
        ;; after. In both cases we wanna add a new bullet item; hence the
        ;; following two separate lines.
	((org-in-item-p) (beginning-of-line) (org-insert-item) (skip-chars-forward " "))
        ((save-excursion (forward-line -1) (org-in-item-p)) (forward-line -1) (beginning-of-line) (org-insert-item) (org-metadown) (end-of-line))
	(t (call-interactively #'acg/smart-open-line-above))))

(defun acg/org-open-line ()
  (interactive)
  (org-check-before-invisible-edit 'insert)
  (cond ((org-in-item-p) (let ((p (point))) (acg/soft-kill-line) (beginning-of-line) (org-insert-item) (org-metadown) (end-of-line) (yank) (goto-char p)))
	(t (call-interactively #'acg/open-line))))

(defun acg/org-newline-above ()
  (interactive)
  (org-check-before-invisible-edit 'insert)
  (cond
   ((org-in-item-p) (acg/soft-kill-line) (beginning-of-line) (org-insert-item) (save-excursion (yank)))
   (t (call-interactively #'acg/newline-above))))

(defun acg/org-in-empty-item-p ()
  "Return t if currently in an empty item (one containing just
the bullet point, whitespaces, but no other character)."
  (and (org-in-item-p)
       (save-excursion
         (beginning-of-line)
         (skip-chars-forward "^a-zA-Z\n")
         (looking-at "$"))))

(defun acg/org-return ()
  "docstring"
  (interactive)
  (org-check-before-invisible-edit 'insert)
  (cond ((acg/org-in-empty-item-p) (beginning-of-line) (kill-line) (insert "\n"))
        ((org-in-item-p) (acg/soft-kill-line) (beginning-of-line) (org-insert-item) (org-metadown) (end-of-line) (save-excursion (yank)))
	(t (call-interactively #'org-return))))

(defun acg/org-metaup (&optional arg)
  "Same as `org-metaup' but just move lines up if region is
active."
  (interactive "P")
  (if (or (region-active-p)
          (org-in-src-block-p))
      (acg/move-lines-up arg)
    (org-metaup arg)))

(defun acg/org-metadown (&optional arg)
  "Same as `org-metadown' but just move lines down if region is
active."
  (interactive "P")
  (if (or (region-active-p)
          (org-in-src-block-p))
      (acg/move-lines-down arg)
    (org-metadown arg)))

;; Make org-insert-link smartly parse a link description.
(defun acg/org-link-make-description-function (link desc)
  (cond (desc desc)
        ((string-match-p "http.?://" link)
         (acg/url-get-page-title link))
        (t nil)))
(setq org-link-make-description-function 'acg/org-link-make-description-function)


(defun acg/org-edit-src-save-exit ()
  "Performs both `org-edit-src-save' and `org-edit-src-exit'."
  (interactive)
  (org-edit-src-save)
  (org-edit-src-exit))

(defun acg/org-edit-src-confirm-abort ()
  "Same as `org-edit-src-abort' but asks for confirmation first."
  (interactive)
  (when (or (not (buffer-modified-p))
            (y-or-n-p
             "Abort changes to this Org Src code block?"))
    (org-edit-src-abort)))

;; TODO: Make a macro that automatically creates the functions below.

(defun acg/org-forward-paragraph ()
  "Circumvent Org mode's overriding of `forward-paragraph'."
  (interactive)
  (org-forward-paragraph))

(defun acg/org-backward-paragraph ()
  "Circumvent Org mode's overriding of `backward-paragraph'."
  (interactive)
  (org-backward-paragraph))


;;; Indentation

;; Don't indent if previous blank lines are not
(add-hook
 'org-mode-hook
 (lambda () (setq acg/electric-indent-newline-as-previous-if-blank t)))

;; Make M-> and M-< operate on whole region lines
(advice-add 'org-shiftmetaright :around #'acg/with-expanded-region-to-whole-lines-noargs)
(advice-add 'org-shiftmetaleft :around #'acg/with-expanded-region-to-whole-lines-noargs)

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

;; (define-key org-mode-map (kbd "C-RET") 'smart-org-meta-return-dwim)
;; (define-key org-mode-map (kbd "C-S-<return>") 'smart-org-insert-todo-heading-dwim)
;; (define-key org-mode-map (kbd "M-<return>") 'smart-org-insert-heading-respent-content-dwim)
;; (define-key org-mode-map (kbd "M-S-<return>") 'smart-org-insert-todo-heading-respect-content-dwim)


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


;; Org-download facilitates saving images
(use-package org-download
  :init
  (setq org-download-image-dir "./img/")
  (setq org-download-heading-lvl nil)
  :hook ((org-mode . org-download-enable)
         (dired-mode . org-download-enable))
  :bind
  (:map org-mode-map
   ("C-c i i" . org-download-image)
   ("C-c i r" . org-download-rename-at-point)
   ("C-c i s" . org-download-screenshot)
   :map dired-mode-map
   ("C-c i i" . org-download-image)
   ("C-c i r" . org-download-rename-at-point)
   ("C-c i s" . org-download-screenshot)))

;; Org YouTube makes it possible to exhibit Images from the web and YouTube videos
;; with `org-toggle-inline-images'.
(use-package org-yt
  :straight (:host github :repo "TobiasZawada/org-yt")
  :config
  ;; Make external images work in Org mode
  (defun org-image-link (protocol link _description)
    "Interpret LINK as base64-encoded image data."
    (cl-assert (string-match "\\`img" protocol) nil
               "Expected protocol type starting with img")
    (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
      (cl-assert buf nil
                 "Download of image \"%s\" failed." link)
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (buffer-substring-no-properties (point) (point-max)))))

  (org-link-set-parameters
   "imghttp"
   :image-data-fun #'org-image-link)

  (org-link-set-parameters
   "imghttps"
   :image-data-fun #'org-image-link))


;; Org SRC blocks

;; Flowcharts
(use-package ob-mermaid)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)
   ;; Org babel Jupyter integration -- Jupyter must be last
   (jupyter . t)
   ;; Orb babel Mermaid integration
   (mermaid . t)))

(setq org-babel-default-header-args:jupyter-python
      '((:async . "yes")
        (:session . "py")
        (:kernel . "python3")
        ))

;; Do not asking when evaluating code
(setq org-confirm-babel-evaluate nil)

;; Do not alter indentation in Org source blocks
(setq org-src-preserve-indentation t)
