(ensure-package-installed 'helm ;; configured in /minor-modes-config/
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
                          'form-feed
                          
			  ;; configured in /minor-modes-config/acg-company-mode.el
			  'company
			  'company-quickhelp
                          'company-anaconda

			  ;; configured in /major-modes-config/
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
			  'smooth-scrolling
			  'which-key
                          'rainbow-mode
                          'auto-dim-other-buffers
			  )

(my-load-all-in-directory "~/.emacs.d/acg-emacs/custom-functions")
(my-load-all-in-directory "~/.emacs.d/acg-emacs/minor-modes-config")
(my-load-all-in-directory "~/.emacs.d/acg-emacs/major-modes-config")

(provide 'acg-packages)
