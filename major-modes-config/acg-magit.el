(require 'magit)

;; disable Emacs's built-in version control since not using it
(setq vc-handled-backends nil)


(with-eval-after-load 'magit
  (global-set-key (kbd "C-x g") 'magit-status)
  ;; (define-key magit-popup-mode-map [escape] 'magit-popup-quit)
  (define-key magit-mode-map [escape] 'magit-mode-bury-buffer)
  (define-key magit-log-mode-map [escape] 'magit-log-bury-buffer)
  (define-key magit-mode-map ["C-h"] nil)
  (define-key magit-log-mode-map ["C-h"] nil)

  (define-key magit-mode-map (kbd "C-w") 'magit-kill-this-buffer)
  )


(require 'transient)

(with-eval-after-load 'transient
  (define-key transient-map (kbd "<escape>") 'transient-quit-all)
  (define-key transient-map (kbd "<escape>") 'transient-quit-all)
  )


;; (global-set-key (kbd "C-x g") 'magit-status)

;; (define-key magit-popup-mode-map [escape] 'magit-popup-quit)
;; (define-key magit-mode-map [escape] 'magit-mode-bury-buffer)
;; (define-key magit-log-mode-map [escape] 'magit-log-bury-buffer)

;; (define-key magit-mode-map (kbd "C-w") 'magit-kill-this-buffer)
