
(use-package scss-mode)
(use-package less-css-mode)
(use-package rainbow-mode)
(use-package yaml-mode)
(use-package scala-mode)
(use-package dockerfile-mode)
(use-package csv-mode)


(use-package markdown-mode
  :config
  (setq markdown-disable-tooltip-prompt t)
  (setq markdown-link-make-text-function 'acg/url-get-page-title)
  (setq markdown-enable-math t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-hide-urls t)
  (setq markdown-hide-markup t)

  (defun acg/markdown-setup ()
    (reftex-mode)
    (setq-local reftex-cite-format "[@%l]")
    (markdown-toggle-url-hiding 1))

  :bind
  (:map markdown-mode-map
        ("C-k" . 'markdown-insert-link))
  :hook
  (markdown-mode . acg/markdown-setup))

;; Dependency for editing Markdown source blocks w/ <C-c '>
(use-package edit-indirect)


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
