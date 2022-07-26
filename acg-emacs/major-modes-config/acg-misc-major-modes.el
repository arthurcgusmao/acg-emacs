;; Tree sitter is a better alternative for parsing syntax trees
(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package tree-sitter-indent)


;; HTML & related
(use-package less-css-mode)
(use-package scss-mode)
(use-package rainbow-mode) ; color color strings (e.g., "#00FF00")

;; DevOps: Docker, Kubernetes, etc.
(use-package docker :bind ("C-c d" . docker))
(use-package dockerfile-mode)
(use-package kubernetes
  :commands (kubernetes-overview)
  :bind
  ("C-c k" . kubernetes-overview))
(use-package conf-mode
  :straight nil
  :mode (("\\.ini$" . conf-windows-mode)
         ("\\.toml$" . conf-toml-mode)
         ("Pipfile$" . conf-toml-mode)))
(use-package yaml-mode)
(use-package hcl-mode)
(use-package terraform-mode)

;; Misc
(use-package csv-mode)
(use-package scala-mode)
(use-package csharp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))
(use-package json-mode
  :config
  (setq js-indent-level 2))
(use-package powershell)

;; Databases
(use-package cql-mode) ; Cassandra


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

  :bind
  (:map markdown-mode-map
        ("C-k" . 'markdown-insert-link)
        ("C-e" . 'markdown-edit-code-block))
  :hook
  (markdown-mode . acg/markdown-setup))

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
        ("C-s" . acg/edit-indirect-save-commit)
        ("C-w" . acg/edit-indirect-abort-confirm)))


(use-package ttl-mode
  :config
  (autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files" t)
  (add-hook 'ttl-mode-hook    ; Turn on font lock when in ttl mode
            'turn-on-font-lock)
  (setq auto-mode-alist
        (append
         (list
          '("\\.n3" . ttl-mode)
          '("\\.ttl" . ttl-mode))
         auto-mode-alist))
  (setq ttl-indent-level 2)
  (setq ttl-electric-punctuation nil))
