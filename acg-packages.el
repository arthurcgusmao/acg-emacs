(ensure-package-installed 'helm
			  'crux
			  'beacon
			  'which-key
			  'undo-tree
			  'anzu
			  'projectile
			  'diminish

			  ;; editor modes
			  'smartparens
			  'scroll-restore
			  'smooth-scrolling
			  'minimap
			  'multiple-cursors
			  
			  ;; 'language' modes
			  'web-mode
			  'scss-mode
			  'less-css-mode
			  'markdown-mode
			  )

(my-load-all-in-directory "~/.emacs.d/acg/modules")
(my-load-all-in-directory "~/.emacs.d/acg/packages-config")

(provide 'acg-packages)
