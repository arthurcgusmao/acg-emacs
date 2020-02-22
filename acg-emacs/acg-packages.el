;; My list of selected packages
(setq acg/selected-packages
      (quote (;; configured in /minor-modes-config/
              crux
              helm
              minimap
              undo-tree
              projectile
              smartparens
              helm-projectile
              helm-swoop
              expand-region
              yasnippet
              yasnippet-snippets
              ivy-yasnippet
              highlight-indent-guides
              tabbar
              form-feed
              neotree
              highlight
              exec-path-from-shell
              ivy
              swiper
              counsel
              smex
              multiple-cursors
              
              ;; configured in /minor-modes-config/acg-company-mode.el
              company
              company-quickhelp
              company-anaconda

              ;; configured in /major-modes-config/
              web-mode
              scss-mode
              less-css-mode
              markdown-mode
              anaconda-mode
              yaml-mode
              scala-mode
              jupyter
              dired-subtree
              magit

              ;; configure in /major-modes-config/acg-term.el
              multi-term
              eterm-256color
              keychain-environment

              ;; configured in /major-modes-config/acg-org-mode.el
              org-ref

              ;; configured in /acg-ui.el
              smooth-scrolling
              which-key
              rainbow-mode
              auto-dim-other-buffers
              ;; themes
              zenburn-theme
              anti-zenburn-theme
              modus-operandi-theme ; by Prot
              modus-vivendi-theme ; by Prot
              )))

;; Integrate `use-package' with `straight.el'
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Install my list of packages
(dolist (p acg/selected-packages)
  (straight-use-package p))

(acg/load-all-in-directory (concat acg/acg-emacs-dir "custom-functions"))
(acg/load-all-in-directory (concat acg/acg-emacs-dir "minor-modes-config"))
(acg/load-all-in-directory (concat acg/acg-emacs-dir "major-modes-config"))

(provide 'acg-packages)
