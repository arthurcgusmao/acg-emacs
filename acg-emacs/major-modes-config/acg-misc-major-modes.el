;; Tree sitter is a better alternative for parsing syntax trees
(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package tree-sitter-indent)


;; HTML & related
(use-package less-css-mode)
(use-package scss-mode)
(use-package rainbow-mode) ; color color strings (e.g., "#00FF00")

;; DevOps: Docker, Kubernetes, etc.
(use-package bazel)
(use-package docker :bind ("C-c d" . docker))
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile_[^\\.]+\\'" . dockerfile-mode)))
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
(use-package json-mode
  :config
  (setq js-indent-level 4))
(use-package powershell)

;; Databases
(use-package cql-mode) ; Cassandra


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
