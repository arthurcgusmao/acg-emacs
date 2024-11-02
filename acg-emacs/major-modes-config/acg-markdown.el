(use-package markdown-mode
  :config
  (setq markdown-disable-tooltip-prompt t)
  (setq markdown-link-make-text-function 'acg/url-get-page-title)
  (setq markdown-enable-math t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-hide-urls t)
  (setq markdown-hide-markup t)

  ;; Inherit faces from Org
  (set-face-attribute 'markdown-code-face nil :inherit '(org-block fixed-pitch))
  (set-face-attribute 'markdown-language-keyword-face nil :inherit 'org-block-begin-line)
  (set-face-attribute 'markdown-inline-code-face nil :inherit 'org-verbatim)

  ;; Add new src (code) block syntaxes
  (add-to-list 'markdown-code-lang-modes '("console" . sh-mode))

  (defun acg/markdown-setup ()
    (reftex-mode)
    (setq-local reftex-cite-format "[@%l]")
    (markdown-toggle-url-hiding 1))

  ;; Enable highlighting, e.g.: ==Highlighted phrase==
  (defun acg/markdown-highlight-setup ()
    "Custom highlighting for ==highlight== in markdown-mode."
    (font-lock-add-keywords nil
                            '(("==\\(.*?\\)=="
                               (0 'hi-yellow t)))))

  :bind
  (:map markdown-mode-map
        ("M-k" . 'markdown-insert-link)
        ("C-e" . 'markdown-edit-code-block))
  :hook
  (markdown-mode . acg/markdown-setup)
  (markdown-mode . acg/markdown-highlight-setup))

;; Dependency for editing Markdown source blocks w/ <C-c '>
(use-package edit-indirect
  :config
  (defun acg/edit-indirect-save-commit ()
    "Same as `edit-indirect-commit' but also saves the original buffer."
    (interactive)
    (edit-indirect-commit)
    (save-buffer))

  (defun acg/edit-indirect-abort-confirm ()
    "Same as `edit-indirect-abort' but asks for confirmation first."
    (interactive)
    (when (or (not (buffer-modified-p))
              (y-or-n-p
               "Abort changes to this `edit-indirect' code block?"))
      (edit-indirect-abort)))

  :bind
  (:map edit-indirect-mode-map
        ("C-c C-c" . nil)
        ("C-e" . edit-indirect-commit)
        ("M-s" . acg/edit-indirect-save-commit)
        ("M-w" . acg/edit-indirect-abort-confirm)))
