;; Integrate `use-package' with `straight.el'
(straight-use-package 'use-package)

;; Make use-package install packages using `straight.el' by default.
(setq straight-use-package-by-default t)
;; Note: to go back to the default behavior (e.g., to configure a built-in
;; package you don't want to build from source), set the keyword `:straight' to
;; `nil' in the use-package declaration.

(acg/load-all-in-directory (concat acg/acg-emacs-dir "custom-functions"))
(acg/load-all-in-directory (concat acg/acg-emacs-dir "minor-modes-config"))
(acg/load-all-in-directory (concat acg/acg-emacs-dir "major-modes-config"))

(provide 'acg-packages)
