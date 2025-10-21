;; Tree sitter is a better alternative for parsing syntax trees
(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package tree-sitter-indent)


;; HTML & related
(use-package less-css-mode)
;; (use-package scss-mode)  ;; Erroring w/ flymake-allowed-file-name-masks.
(use-package rainbow-mode) ; color color strings (e.g., "#00FF00")

;; DevOps: Docker, Kubernetes, etc.
(use-package bazel)
(use-package docker :bind ("C-c d" . docker))
(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile[_-][^\\.]+\\'" . dockerfile-mode)))
(use-package kubernetes
  :commands (kubernetes-overview)
  :bind
  ("C-c k" . kubernetes-overview))
(use-package make-mode
  :config
  (add-to-list 'auto-mode-alist '("Makefile[_-][^\\.]+\\'" . makefile-mode)))
(use-package conf-mode
  :straight nil
  :mode (("\\.ini$" . conf-windows-mode)
         ("\\.toml$" . conf-toml-mode)
         ("Pipfile$" . conf-toml-mode)))
(use-package yaml-mode)
(use-package hcl-mode)
(use-package terraform-mode)
(use-package pkl-mode
  :straight (:host github :protocol ssh
                   :repo "sin-ack/pkl-mode"))

;; Misc
(use-package csv-mode)
(use-package scala-mode)
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
