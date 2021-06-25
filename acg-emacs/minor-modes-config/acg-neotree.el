(use-package neotree
  (:host github :protocol ssh
         :repo "arthurcgusmao/emacs-neotree")

  :config
  (setq neo-window-fixed-size nil)      ; Allow resizing neotree window
  (setq neo-window-width 45)
  (setq neo-smart-open t)               ; Automatically find current file when showing neotree

  :bind
  ("M-d" . neotree-show)
  ("M-D" . neotree-hide))
