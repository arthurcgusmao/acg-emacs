(use-package unmodified-buffer
  :straight (:host github :protocol ssh
                   :repo "arthurcgusmao/unmodified-buffer")
  :config
  (setq unmodified-buffer-ignore-remote t)
  (unmodified-buffer-global-mode 1))
