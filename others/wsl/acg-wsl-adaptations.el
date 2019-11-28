;; adapatations to use Emacs in WSL within MS Windows

;; configure font. For whatever reason Emacs does not start with XFCE4's
;; default monospace font
(defun fontify-frame-wsl (frame)
  (cond
   ((>= (display-pixel-height) 2160) (set-frame-parameter frame 'font "Hack-15")) ; 4k resolution
   ((>= (display-pixel-height) 1440) (set-frame-parameter frame 'font "Hack-10.5")) ; 2560x1440 resolution
   (t (set-frame-parameter frame 'font "Hack-9"))))

(fontify-frame-wsl nil) ;; Fontify current frame
(push 'fontify-frame-wsl after-make-frame-functions) ;; Fontify any future frames
