;; loads acg-emacs
(add-to-list 'load-path (concat user-emacs-directory "acg-emacs"))
(require 'acg-core)

;; Use `local-settings.el' file for any local configurations you might wanna have
(load (concat user-emacs-directory "local-settings.el") t)
