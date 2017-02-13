(require 'magit)

(global-set-key (kbd "C-c g") 'magit-status)

(define-key magit-popup-mode-map [escape] 'magit-popup-quit)
(define-key magit-mode-map [escape] 'magit-mode-bury-buffer)
(define-key magit-log-mode-map [escape] 'magit-log-bury-buffer)

(define-key magit-mode-map (kbd "C-w") 'magit-kill-this-buffer)
