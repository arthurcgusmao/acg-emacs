;; http://emacs-fu.blogspot.com.br/2011/08/customizing-mode-line.html
;; http://amitp.blogspot.com.br/2011/08/emacs-custom-mode-line.html
;; http://www.lunaryorn.com/posts/make-your-emacs-mode-line-more-useful.html

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
              (list
               
               ;; was this buffer modified since the last save?
               '(:eval (if buffer-read-only
                         (propertize " [RO]"
                                     'face 'custom-ml-buffer-readonly)
                         (propertize (if (buffer-modified-p) " [M]" " [S]")
                                     'face 'custom-ml-buffer-modified
                                     'help-echo "Buffer modified/saved/read-only")))
               
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b" 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))
               

               ;; line and column
               ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize " (%02l," 'face 'font-lock-type-face)
               (propertize "%02c)" 'face 'font-lock-type-face) 

               ;; relative position, size of file
               (propertize " [%p]" 'face 'font-lock-type-face) ;; % above top
               

               ;; the current major mode for the buffer.
               '(:eval (propertize " %m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))

               '(vc-mode vc-mode)
               
               
               ;; projectile indicator
               '(:eval (format " [Proj: %s]" (projectile-project-name)))
               
               
               minor-mode-alist  ;; list of minor modes
               ))


(make-face 'custom-ml-buffer-modified)
(set-face-attribute 'custom-ml-buffer-modified nil
                    ;; :background "transparent"
                    :foreground (face-attribute 'font-lock-comment-delimiter-face :foreground)
                    ;; :height 80
                    :weight 'bold
                    ;; :box '(:line-width -4 :color "black")
                    )

(make-face 'custom-ml-buffer-readonly)
(set-face-attribute 'custom-ml-buffer-readonly nil
                    ;; :background "transparent"
                    :foreground (face-attribute 'font-lock-comment-face :foreground)
                    ;; :height 80
                    :weight 'bold
                    ;; :box '(:line-width -4 :color "black")
                    )

