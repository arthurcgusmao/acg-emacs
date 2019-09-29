(deftheme acg-common-faces
  "A theme for loading common faces to other themes.")

(custom-theme-set-faces
 'acg-common-faces
 
 ;; mode line
 ;; '(mode-line
 ;;   ((t (:background "#203" :foreground "#ddd" :box
 ;;                    (:line-width 2 :color "#203" :style released-button)))))
 '(mode-line-inactive
   ((t (:inherit mode-line :background "#555"
                 :foreground "#bbb" :box (:line-width 1 :color "#777")
                 :weight light))))
 
 ;; minimap
 '(minimap-active-region-background
   ((((background dark)) (:background "#2A2A2A222222"))
    (t (:background "#D3D3D3222222"))) nil :group)
 
 ;; markdown and org-mode
 '(markdown-header-face-1 ((t (:inherit outline-1 :foreground "#0CF" :weight bold :height 1.5))))
 '(markdown-header-face-2 ((t (:inherit outline-2 :foreground "#0EA" :weight bold :height 1.3))))
 '(markdown-header-face-3 ((t (:inherit outline-3 :foreground "#9E0" :weight bold :height 1.15))))
 '(markdown-header-face-4 ((t (:inherit outline-4 :foreground "#CC0" :weight bold :height 1.0))))
 '(markdown-header-face-5 ((t (:inherit outline-5 :foreground "#EA0" :weight bold :height 1.0))))
 '(markdown-header-face-6 ((t (:inherit outline-6 :foreground "#D60" :weight bold :height 1.0))))
 '(markdown-header-face-7 ((t (:inherit outline-7 :foreground "#90E" :weight bold :height 1.0))))
 '(markdown-header-face-8 ((t (:inherit outline-8 :foreground "#0FF" :weight bold :height 1.0))))
 '(org-level-1 ((t (:inherit outline-1 :foreground "#0CF" :weight bold :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "#0EA" :weight bold :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "#9E0" :weight bold :height 1.15))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "#CC0" :weight bold :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "#EA0" :weight bold :height 1.0))))
 '(org-level-6 ((t (:inherit outline-6 :foreground "#D60" :weight bold :height 1.0))))
 '(org-level-7 ((t (:inherit outline-7 :foreground "#90E" :weight bold :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :foreground "#0FF" :weight bold :height 1.0))))
 
 ;; parent matching
 ;; '(show-paren-match
 ;;   ((t (:background "#085" :foreground "white"))))
 ;; '(show-paren-mismatch
 ;; ((t (:background "#E04151" :foreground "white"))))
 '(show-paren-match
   ((t (:foreground "green" :weight bold))))
 '(show-paren-mismatch
   ((t (:foreground "red" :weight bold))))

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

 ;; fringe
 '(fringe ((t (:foreground "orange" :background "#3d3c3a"))))
 ;; border (buffer separator)
 '(vertical-border ((t (:foreground "#3d3c3a"))))
 ;; linum
 '(linum ((t (:inherit fringe :weight normal :foreground "#6d6c6a"))))
 )


(provide-theme 'acg-common-faces)
