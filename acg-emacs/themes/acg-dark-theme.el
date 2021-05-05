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

 ;; mode line
 '(mode-line
   ((t (:background "#203" :foreground "#ddd" :box
                    (:line-width 2 :color "#203" :style released-button)))))
 '(mode-line-inactive
   ((t (:inherit mode-line :background "#555"
                 :foreground "#bbb" :box (:line-width 1 :color "#777")
                 :weight light))))

 ;; emacs-jupyter
 ;; '(jupyter-eval-overlay ((t (:background "#000066" :foreground "#9999FF"
 ;;                                         :height 0.8))))
 '(jupyter-eval-overlay ((t (:background "#621e04" :foreground "#F87440"
                                         :height 0.8))))

 ;; Org-mode
 ;; custom fonts for src begin/end
 '(org-block
   ((t (:extend t :height 0.9 :background "#050528"))))
 '(org-meta-line
   ((t (:inherit font-lock-comment-face :extend t :height 0.67 :background "#050528" :foreground "#3C3C6C"))))
 '(org-verbatim
   ((t (:inherit font-lock-constant-face :height 0.9 :family "Monospace"))))
 '(org-code
   ((t (:height 0.8 :inherit font-lock-constant-face :family "Monospace"))))
 '(org-quote
   ((t (:inherit (variable-pitch org-block) :slant italic))))

 ;; fringe
 '(fringe ((t (:foreground "orange" :background "#3d3c3a"))))
 ;; border (buffer separator)
 '(vertical-border ((t (:foreground "#3d3c3a"))))
 )


(provide-theme 'acg-dark)
