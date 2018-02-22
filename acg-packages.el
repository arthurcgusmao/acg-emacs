(ensure-package-installed 'helm ;; configured in /minor-modes-config/
			  'crux
                          'use-package
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
                          'fix-word
                          'magit
                          'ein
                          'highlight
                          'exec-path-from-shell
                          
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
                          'yaml-mode

                          ;; configured in /major-modes-config/acg-org-mode.el
                          'org-ref

			  ;; configured in /acg-editor.el
			  'anzu
			  'multiple-cursors

                          ;; configured in /acg-ui.el
			  'smooth-scrolling
			  'which-key
                          'rainbow-mode
                          'auto-dim-other-buffers
			  )

(my-load-all-in-directory (concat acg-emacs-dir "custom-functions"))
(my-load-all-in-directory (concat acg-emacs-dir "minor-modes-config"))
(my-load-all-in-directory (concat acg-emacs-dir "major-modes-config"))

(provide 'acg-packages)
