(deftheme acg-light "A light theme.")

(load-theme 'acg-common-faces)

(set-face-background 'hl-line "#FFFFFF")
(set-cursor-color "#000000")

(custom-theme-set-faces
 'acg-light

 '(default
    ((t (:inherit nil :stipple nil :background "#f0f0f0"
                  :foreground "#050505" :inverse-video nil :box nil
                  :strike-through nil :overline nil :underline nil
                  :slant normal :weight normal))))
 ;; '(font-lock-builtin-face ((t (:foreground "#88d"))))
 '(font-lock-builtin-face ((t (:foreground "#b54040"))))
 '(font-lock-comment-face ((t (:foreground "#969896" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#006666" :weight bold))))
 ;; '(font-lock-function-name-face ((t (:foreground "#7F33A9"))))
 ;; '(font-lock-function-name-face ((t (:foreground "#9D1353"))))
 '(font-lock-function-name-face ((t (:foreground "#0086b3"))))
 ;; '(font-lock-type-face ((t (:foreground "#c27100"))))
 '(font-lock-type-face ((t (:foreground "purple"))))
 '(font-lock-keyword-face ((t (:foreground "#19198f"))))
 ;; '(font-lock-string-face ((t (:foreground "#15309F"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#9e4f00"))))
 '(font-lock-string-face ((t (:foreground "#b85c00"))))
 '(font-lock-variable-name-face ((t (:foreground "#1d814c"))))
 '(region ((t (:background "#a4d8e5"))))

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
   ((t (:background "#ab89c2" :foreground "#203" :box
                    (:line-width 2 :color "#ab89c2" :style released-button)))))

 ;; emacs-jupyter
 '(jupyter-eval-overlay ((t (:background "#F87440" :foreground "#621e04"
                                         :height 0.8))))

 ;; Org-mode
 ;; custom fonts for src begin/end
 '(org-block
   ((t (:extend t :height 0.9 :background "#aaa9d2"))))
 '(org-meta-line
   ((t (:inherit font-lock-comment-face :extend t :height 0.67 :background "#aaa9d2" :foreground "#7f7fa6"))))
 '(org-verbatim
   ((t (:inherit font-lock-constant-face :height 0.9 :family "Monospace"))))
 '(org-code
   ((t (:height 0.8 :inherit font-lock-constant-face :family "Monospace"))))
 '(org-quote
   ((t (:inherit (variable-pitch org-block) :slant italic))))

 )


(provide-theme 'acg-light)
