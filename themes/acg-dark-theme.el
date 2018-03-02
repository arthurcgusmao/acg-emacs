(deftheme acg-dark "A dark theme.")

(load-theme 'acg-common-faces t)

(set-face-background 'hl-line "#000000")
(set-cursor-color "#FFFFFF")

(custom-theme-set-faces
 'acg-dark
 
 '(default
    ((t (:inherit nil :stipple nil :background "#1B1B2A" :foreground "#ddd"
                  :inverse-video nil :box nil :strike-through nil
                  :overline nil :underline nil :slant normal :weight normal))))
 '(font-lock-builtin-face ((t (:foreground "#7ce"))))
 ;; '(font-lock-comment-face ((t (:foreground "#666" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "#648" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#7ec"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#88d"))))
 '(font-lock-function-name-face ((t (:foreground "#bb55ff" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#50bfff" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#d88"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#ddcc82"))))
 '(region ((t (:background "#66a"))))

 ;; helm
 '(helm-buffer-directory ((t (:inherit helm-ff-directory))))
 '(helm-buffer-file ((t (:foreground "#ddd"))))
 '(helm-buffer-process ((t (:foreground "dark slate gray"))))
 '(helm-ff-directory ((t (:background "#000" :foreground "#B1DBFF"))))
 '(helm-ff-dotted-directory
   ((t (:inherit helm-ff-directory :foreground "#555"))))
 '(helm-ff-file
   ((t (:inherit font-lock-builtin-face :foreground "#ddd"))))
 '(helm-source-header
   ((t (:inherit 'fringe :weight bold :height 1.4 :family "Sans Serif"
                 :box (:line-width 10 :color "#3d3c3a")))))


 ;; multiple cursors
 '(mc/cursor-bar-face ((t (:height 1 :background "white"))))
 )


(provide-theme 'acg-dark)
