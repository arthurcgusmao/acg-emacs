;; http://emacs-fu.blogspot.com.br/2011/08/customizing-mode-line.html
;; http://amitp.blogspot.com.br/2011/08/emacs-custom-mode-line.html
;; http://www.lunaryorn.com/posts/make-your-emacs-mode-line-more-useful.html

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
              (list
               " "
               ;; was this buffer modified since the last save?
               '(:eval (if buffer-read-only
                           (propertize ""
                                       'face 'acg-ml-buffer-readonly)
                         (propertize (if (buffer-modified-p) "[m]" "[s]")
                                     'face 'acg-ml-buffer-modified
                                     'help-echo "Buffer modified/saved/read-only")))
               
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b" 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))
               

               ;; line and column
               ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "   L%1l " 'face 'font-lock-type-face)
               (propertize "C%1c" 'face 'font-lock-type-face) 
               ;; relative position, size of file
               (propertize " %p" 'face 'font-lock-type-face) ;; % above top
               

               ;; the current major mode for the buffer.
               '(:eval (propertize "   %m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))

               ;;(if (vc-registered (buffer-file-name))
               ;;    "  ")
	       "   "
               '(vc-mode vc-mode)
               
               ;; projectile indicator
               '(:eval (format "   Proj:%s   " (projectile-project-name)))
               
               
               minor-mode-alist  ;; list of minor modes
               ))


(make-face 'acg-ml-buffer-modified)
(set-face-attribute 'acg-ml-buffer-modified nil
                    :foreground "#777"
                    :weight 'regular
                    )

(make-face 'acg-ml-buffer-readonly)
(set-face-attribute 'acg-ml-buffer-readonly nil
                    ;; :background "transparent"
                    :foreground (face-attribute 'font-lock-comment-face :foreground)
                    ;; :height 80
                    :weight 'bold
                    ;; :box '(:line-width -4 :color "black")
                    )

(make-face 'acg-ml-buffer-position)
(set-face-attribute 'acg-ml-buffer-position nil
                    ;; :background "transparent"
                    :foreground (face-attribute 'font-lock-comment-face :foreground)
                    ;; :height 80
                    :weight 'bold
                    ;; :box '(:line-width -4 :color "black")
                    )

(make-face 'acg-test)
(set-face-attribute 'acg-test nil
                    ;; :background "transparent"
                    :foreground "#888"
                    :height 0.8
                    )
