(deftheme acg-light "A light theme.")

(load-theme 'acg-common-faces)

(set-face-background 'hl-line "#fff")
(set-cursor-color "#000")

(custom-theme-set-faces
 'acg-light

 '(default
    ((t (:inherit nil :stipple nil :background "#f3f3f3" :foreground "#141414"
                  :inverse-video nil :box nil :strike-through nil
                  :overline nil :underline nil :slant normal :weight normal))))
 '(font-lock-builtin-face ((t (:foreground "#a22"))))
 '(font-lock-comment-face ((t (:foreground "#999" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#066" :weight bold))))
 ;; '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#a18365"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#a78"))))
 '(font-lock-function-name-face ((t (:foreground "#08a" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#118" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#940"))))
 '(font-lock-type-face ((t (:foreground "#80d" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#063"))))
 '(region ((t (:background "#add"))))

 '(helm-buffer-directory ((t (:inherit helm-ff-directory))))
 '(helm-buffer-file ((t (:foreground "black"))))
 '(helm-buffer-process ((t (:foreground "light slate gray"))))
 '(helm-ff-directory ((t (:background "gainsboro" :foreground "#4b2500"))))
 '(helm-ff-dotted-directory
   ((t (:inherit helm-ff-directory :foreground "gray"))))
 '(helm-ff-file
   ((t (:inherit font-lock-builtin-face :foreground "black"))))
 '(helm-source-header
   ((t (:background "dark gray" :foreground "gray10"
                    :box (:line-width 5 :color "grey75") :weight bold
                    :height 1.3 :family "Sans Serif"))))

 ;; multiple cursors
 '(mc/cursor-bar-face ((t (:height 1 :background "black"))))

 ;; ;; fringe
 ;; '(fringe ((t (:foreground "orange" :background "gray"))))
 ;; ;; border (buffer separator)
 ;; '(vertical-border ((t (:foreground "gray"))))

 '(mode-line
   ((t (:background "#b9c" :foreground "#203" :box
                    (:line-width 2 :color "#b9c" :style released-button)))))
 '(mode-line-inactive
   ((t (:inherit mode-line :background "#ccc"
                 :foreground "#555" :box (:line-width 1 :color "#999")
                 :weight light))))

 ;; emacs-jupyter
 '(jupyter-eval-overlay ((t (:background "#F87440" :foreground "#621e04"
                                         :height 0.8))))

 ;; Org-mode
 ;; custom fonts for src begin/end
 '(org-block
   ((t (:extend t :height 0.9 :background "#e9e9df"))))
 '(org-meta-line
   ;; ((t (:inherit font-lock-comment-face :extend t :height 0.67 :background "#e9e9df" :foreground "#bfbfb7"))))
   ((t (:inherit font-lock-comment-face :extend t :height 0.67 :foreground "#b1b1a7"))))
 '(org-verbatim
   ((t (:inherit (fixed-pitch font-lock-constant-face) :height 0.9))))
 '(org-code
   ((t (:inherit (fixed-pitch font-lock-constant-face) :height 0.8))))
 '(org-quote
   ((t (:inherit (variable-pitch org-block) :foreground "#333" :slant italic))))

 ;; fringe
 '(fringe ((t (:foreground "orange" :background "#d3d2d0"))))
 ;; border (buffer separator)
 '(vertical-border ((t (:foreground "#d3d2d0"))))

 ;; tab-line
 '(tab-line ((t (:inherit default :inherit variable-pitch :background "#d3d2d0" :foreground "#777" :overline nil :underline nil :height 0.9))))
 '(tab-line-tab ((t (:inherit tab-line :background "#e9e5e0" :foreground "#444342"))))
 '(tab-line-tab-inactive ((t (:inherit tab-line-tab :background "#b9b5b0"))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab :background "#f9f5f0" :foreground "#191817"))))
 '(tab-line-highlight ((t (:inherit tab-line-tab :background "#FA9"))))
 )


(provide-theme 'acg-light)
