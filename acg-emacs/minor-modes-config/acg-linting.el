(use-package flycheck
  ;; :hook
  ;; ;; Flycheck works out-of-the-box with flake8, pylint, and pycompile.
  ;; (python-mode . (lambda () (flycheck-mode +1)))
  )


(use-package flycheck-grammarly
  :config
  (setq flycheck-grammarly-check-time 0.8))
