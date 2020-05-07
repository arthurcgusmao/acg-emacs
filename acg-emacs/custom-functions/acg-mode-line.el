;; (use-package projectile)

;; http://emacs-fu.blogspot.com.br/2011/08/customizing-mode-line.html
;; http://amitp.blogspot.com.br/2011/08/emacs-custom-mode-line.html
;; http://www.lunaryorn.com/posts/make-your-emacs-mode-line-more-useful.html

(make-face 'acg/mode-line-common)
(set-face-attribute 'acg/mode-line-common nil
                    ;; :foreground "#777"
                    :foreground (face-attribute 'mode-line :foreground)
                    :weight 'ultralight
                    )
(make-face 'acg/mode-line-common-bold)
(set-face-attribute 'acg/mode-line-common-bold nil
                    :foreground (face-attribute 'acg/mode-line-common :foreground)
                    :weight 'bold
                    )

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
              (list
               "%n " ; Print "Narrow" if region is narrowed
               ;; was this buffer modified since the last save?
               '(:eval (if buffer-read-only
                           " "
                         (if (buffer-modified-p) " 🖉" " 💾")))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b" 'face 'acg/mode-line-common-bold))

               ;; Line & Column numbers -- Use '%02l' to set to 2 chars at least; may prevent flickering
               "   (%l,%c)"
               ;; Relative position, size of file
               " %p" ;; % above top
               ;; Current major mode for the buffer.
               "   📃 %m"

               ;; ;; Project indication -- Do not show for remote files
               ;; "  "
               ;; ;; Projectile indicator
               ;; ;; '(:eval (format " 📁 %s" (projectile-project-name)))
               ;; ;; version control indicator
               ;; '(:eval (when (stringp vc-mode)
	       ;;           '(" 🔀" (vc-mode vc-mode))))

               ;; list of minor modes
               ;; minor-mode-alist
               ))
