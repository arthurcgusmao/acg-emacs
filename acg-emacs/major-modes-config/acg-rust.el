(use-package rust-mode
  :config
  (setq rust-format-on-save t)

  (defun rust-setup ()
    (setq indent-tabs-mode nil)
    (prettify-symbols-mode))

  :hook (rust-mode rust-setup))
