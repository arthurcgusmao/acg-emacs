(deftheme acg-dark "A dark theme.")

(load-theme 'acg-common-faces t)

(set-cursor-color "#FFFFFF")

(custom-theme-set-faces
 'acg-dark

 '(default
    ((t (:inherit nil :stipple nil :background "#1B1B2A" :foreground "#ddd"
                  :inverse-video nil :box nil :strike-through nil
                  :overline nil :underline nil :slant normal :weight normal))))
 '(hl-line ((t (:background "#000000"))))
 '(font-lock-builtin-face ((t (:foreground "#7ce"))))
 '(font-lock-comment-face ((t (:foreground "#648" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#7ec"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#88d"))))
 '(font-lock-function-name-face ((t (:foreground "#bb55ff" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#50bfff"))))
 '(font-lock-string-face ((t (:foreground "#d88"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#dc8"))))
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
   ((t (:background "#5f4388" :foreground "#ffffff" :box (:line-width 3 :style flat-button)))))
 '(mode-line-inactive
   ((t (:background "#736488" :foreground "#c8c1d4" :box (:line-width 3 :style flat-button)))))

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
   ((t (:inherit (fixed-pitch font-lock-constant-face) :height 0.9))))
 '(org-code
   ((t (:inherit (fixed-pitch font-lock-constant-face) :height 0.8))))
 '(org-quote
   ((t (:inherit (variable-pitch org-block) :foreground "#bbb" :slant italic))))

 ;; fringe
 '(fringe ((t (:foreground "orange" :background "#3d3c3a"))))
 ;; border (buffer separator)
 '(vertical-border ((t (:foreground "#3d3c3a"))))

 ;; tab-line
 '(tab-line ((t (:inherit default :inherit variable-pitch :background "#3d3c3a" :foreground "#999" :overline nil :underline nil :height 0.9))))
 '(tab-line-tab ((t (:inherit tab-line :background "#8d8986" :foreground "#333"))))
 '(tab-line-tab-inactive ((t (:inherit tab-line-tab :background "#7d7a77"))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab :background "#BBBBBF"))))
 '(tab-line-highlight ((t (:inherit tab-line-tab :background "#FA9"))))
 )


(provide-theme 'acg-dark)
