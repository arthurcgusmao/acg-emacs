(ensure-package-installed 'helm
			  'crux
			  'beacon
			  'which-key
			  'minimap
			  'undo-tree
			  'projectile
			  'smartparens
			  'helm-projectile
			  'diminish

			  ;; editor modes
			  'anzu
			  'scroll-restore
			  'smooth-scrolling
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
