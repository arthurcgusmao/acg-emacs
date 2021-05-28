(use-package unmodified-buffer
  :straight (:host github :protocol ssh
                   :repo "arthurcgusmao/unmodified-buffer")
  :hook (after-init . unmodified-buffer-mode))
