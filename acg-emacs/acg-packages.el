;; Integrate `use-package' with `straight.el'
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(acg/load-all-in-directory (concat acg/acg-emacs-dir "custom-functions"))
(acg/load-all-in-directory (concat acg/acg-emacs-dir "minor-modes-config"))
(acg/load-all-in-directory (concat acg/acg-emacs-dir "major-modes-config"))

(provide 'acg-packages)
