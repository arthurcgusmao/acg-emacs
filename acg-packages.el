(require 'package)

;; Adding Repositories
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setq package-selected-packages
      (quote (;; configured in /minor-modes-config/
              use-package
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

;; initialize and install selected packages (cf. https://stackoverflow.com/a/39891192/5103881)
(setq package-enable-at-startup nil)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(acg/load-all-in-directory (concat acg/acg-emacs-dir "custom-functions"))
(acg/load-all-in-directory (concat acg/acg-emacs-dir "minor-modes-config"))
(acg/load-all-in-directory (concat acg/acg-emacs-dir "major-modes-config"))

(provide 'acg-packages)
