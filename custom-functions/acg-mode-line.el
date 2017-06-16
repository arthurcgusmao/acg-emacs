;; http://emacs-fu.blogspot.com.br/2011/08/customizing-mode-line.html
;; http://amitp.blogspot.com.br/2011/08/emacs-custom-mode-line.html
;; http://www.lunaryorn.com/posts/make-your-emacs-mode-line-more-useful.html

(make-face 'acg-mode-line-common)
(set-face-attribute 'acg-mode-line-common nil
                    ;; :foreground "#777"
                    :foreground (face-attribute 'mode-line :foreground)
                    :weight 'ultralight
                    )
(make-face 'acg-mode-line-common-bold)
(set-face-attribute 'acg-mode-line-common-bold nil
                    :foreground (face-attribute 'acg-mode-line-common :foreground)
                    :weight 'bold
                    )

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
              (list
               " "
               ;; was this buffer modified since the last save?
               '(:eval (if buffer-read-only
                           (propertize ""
                                       'face 'acg-mode-line-common)
                         (propertize (if (buffer-modified-p) "[m]" "[s]")
                                     'face 'acg-mode-line-common
                                     'help-echo "Buffer modified/saved/read-only")))
               
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b" 'face 'acg-mode-line-common-bold
                                   'help-echo (buffer-file-name)))
               

               ;; line and column
               ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "   L%1l " 'face 'acg-mode-line-common)
               (propertize "C%1c" 'face 'acg-mode-line-common) 
               ;; relative position, size of file
               (propertize " %p" 'face 'acg-mode-line-common) ;; % above top
               

               ;; the current major mode for the buffer.
               '(:eval (propertize "   %m" 'face 'acg-mode-line-common
                                   'help-echo buffer-file-coding-system))

               ;;(if (vc-registered (buffer-file-name))
               ;;    "  ")
	       "   "
               '(vc-mode vc-mode)
               
               ;; projectile indicator
               '(:eval (format "   Proj:%s   " (projectile-project-name)))
               
               
               ;; minor-mode-alist  ;; list of minor modes
               ))
