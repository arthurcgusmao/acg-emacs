(deftheme acg-light "A light theme.")

(load-theme 'acg-common-faces)

(set-face-background 'hl-line "#FFFFFF")
(set-cursor-color "#000000")

(custom-theme-set-faces
 'acg-light

 '(default
    ((t (:inherit nil :stipple nil :background "#eee"
                  :foreground "#222" :inverse-video nil :box nil
                  :strike-through nil :overline nil :underline nil
                  :slant normal :weight normal))))
 '(font-lock-builtin-face ((t (:foreground "#88d"))))
 '(font-lock-builtin-face ((t (:foreground "indian red"))))
 '(font-lock-comment-face ((t (:foreground "#969896" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#055"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "indian red"))))
 ;; '(font-lock-function-name-face ((t (:foreground "#7F33A9"))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((t (:foreground "#9D1353"))))
 ;; '(font-lock-string-face ((t (:foreground "#15309F"))))
 '(font-lock-string-face ((t (:foreground "#66b"))))
 '(font-lock-type-face ((t (:foreground "#0086b3"))))
 '(font-lock-variable-name-face ((t (:foreground "gold4"))))

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

  ;; fringe
 '(fringe ((t (:foreground "orange" :background "gray"))))
 ;; border (buffer separator)
 '(vertical-border ((t (:foreground "gray"))))
 )


(provide-theme 'acg-light)
