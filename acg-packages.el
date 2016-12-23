(ensure-package-installed 'helm ;; configured in /packages-config/
			  'crux
			  'minimap
			  'undo-tree
			  'projectile
			  'smartparens
			  'helm-projectile
			  'expand-region
			  'diminish
                          'yasnippet
                          'highlight-indent-guides
                          'tabbar
                          ;; 'tabbar-ruler
                          
			  ;; configured in /packages-config/company-mode.el
			  'company
			  'company-quickhelp
                          'company-anaconda

			  ;; configured in /languages-config/
			  'web-mode
			  'scss-mode
			  'less-css-mode
			  'markdown-mode
                          'anaconda-mode

			  ;; configured in /acg-editor.el
			  'anzu
			  'multiple-cursors
			  'smartparens

                          ;; configured in /acg-ui.el
			  'beacon
			  'smooth-scrolling
			  ;; 'scroll-restore
			  'which-key
                          'rainbow-mode
			  )

(my-load-all-in-directory "~/.emacs.d/acg-emacs/custom-functions")
(my-load-all-in-directory "~/.emacs.d/acg-emacs/minor-modes-config")
(my-load-all-in-directory "~/.emacs.d/acg-emacs/major-modes-config")

(provide 'acg-packages)
