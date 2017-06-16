(defun acg-apply-common-faces ()
  "Applies faces to make a dark theme."
  (interactive)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.

   ;; mode line
   '(mode-line
     ((t (:background "#405" :foreground "#ddd" :box
                      (:line-width 2 :color "#405" :style released-button)))))
   '(mode-line-inactive
     ((t (:inherit mode-line :background "#555"
                   :foreground "#bbb" :box (:line-width 1 :color "grey75")
                   :weight light))))
   
   ;; minimap
   '(minimap-active-region-background
     ((((background dark)) (:background "#2A2A2A222222"))
      (t (:background "#D3D3D3222222"))) nil :group)
   
   ;; markdown and org-mode
   '(markdown-header-face-1 ((t (:inherit outline-1 :foreground "#00F" :weight bold :height 1.5))))
   '(markdown-header-face-2 ((t (:inherit outline-2 :foreground "#0AF" :weight bold :height 1.3))))
   '(markdown-header-face-3 ((t (:inherit outline-3 :foreground "#0FA" :weight bold :height 1.15))))
   '(markdown-header-face-4 ((t (:inherit outline-4 :foreground "#9E0" :weight bold :height 1.0))))
   '(markdown-header-face-5 ((t (:inherit outline-5 :foreground "#CC0" :weight bold :height 1.0))))
   '(markdown-header-face-6 ((t (:inherit outline-6 :foreground "#EA0" :weight bold :height 1.0))))
   '(markdown-header-face-7 ((t (:inherit outline-7 :foreground "#D60" :weight bold :height 1.0))))
   '(markdown-header-face-8 ((t (:inherit outline-8 :foreground "#90E" :weight bold :height 1.0))))
   '(org-level-1 ((t (:inherit outline-1 :foreground "#00F" :weight bold :height 1.5))))
   '(org-level-2 ((t (:inherit outline-2 :foreground "#0AF" :weight bold :height 1.3))))
   '(org-level-3 ((t (:inherit outline-3 :foreground "#0FA" :weight bold :height 1.15))))
   '(org-level-4 ((t (:inherit outline-4 :foreground "#9E0" :weight bold :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :foreground "#CC0" :weight bold :height 1.0))))
   '(org-level-6 ((t (:inherit outline-6 :foreground "#EA0" :weight bold :height 1.0))))
   '(org-level-7 ((t (:inherit outline-7 :foreground "#D60" :weight bold :height 1.0))))
   '(org-level-8 ((t (:inherit outline-8 :foreground "#90E" :weight bold :height 1.0))))
   
   ;; parent matching
   '(show-paren-match
     ((t (:background "#085" :foreground "white"))))
   '(show-paren-mismatch
     ((t (:background "#E04151" :foreground "white"))))
   
   ;; tabbar
   '(tabbar-button ((t (:inherit tabbar-default :weight normal))))
   '(tabbar-button-highlight ((t (:inherit tabbar-default))))
   '(tabbar-default ((t (:inherit default :background "#3d3c3a" :foreground
                                  "#999":weight medium :overline nil :underline
                                  nil :height 0.9 :widthtype semi-condensed))))
   '(tabbar-highlight ((t (:inherit tabbar-default :background "#FA9"))))
   '(tabbar-modified ((t (:inherit tabbar-default))))
   '(tabbar-selected
     ((t (:inherit tabbar-default :background "#BBBBBF" :foreground "#333"))))
   '(tabbar-selected-modified ((t (:inherit tabbar-selected))))
   '(tabbar-separator ((t (:inherit tabbar-default :background "#333"))))
   '(tabbar-unselected ((t (:inherit tabbar-default))))
   ))
