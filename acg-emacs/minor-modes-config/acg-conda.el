(use-package conda
  :init
  (setq conda-anaconda-home
        (expand-file-name "~/.miniconda3/"))
  :config
  (conda-env-initialize-eshell)
  ;; (conda-env-autoactivate-mode t)
  ;; (conda-env-autoactivate-mode)
  (conda-env-activate-path conda-anaconda-home))
