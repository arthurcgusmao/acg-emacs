(ensure-package-installed 'helm
			  'crux
			  'beacon
			  'which-key
			  'undo-tree
			  'anzu
			  'projectile
			  'helm-projectile
			  'diminish

			  ;; editor modes
			  'smartparens
			  'scroll-restore
			  'smooth-scrolling
			  'minimap
			  'multiple-cursors
			  'expand-region
			  'smartparens
			  
			  ;; 'language' modes
			  'web-mode
			  'scss-mode
			  'less-css-mode
			  'markdown-mode

			  ;; company autocomplete modes
			  'company
			  'company-quickhelp
			  )

(my-load-all-in-directory "~/.emacs.d/acg/modules")
(my-load-all-in-directory "~/.emacs.d/acg/packages-config")

(provide 'acg-packages)
