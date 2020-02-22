(defun acg/super-keyboard-quit ()
  "Run many similar quit commands to make this function a general quit."
  (interactive)
  (keyboard-quit)
  (abort-recursive-edit)
  (setq quit-flag t)
  )

;; keybindings

;;; esc always quits
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(eval-after-load "help-mode" '(define-key help-mode-map [escape] 'kill-buffer-and-window))
(eval-after-load "anaconda-mode-view-mode" '(define-key anaconda-mode-view-mode-map [escape] 'kill-buffer-and-window))
(global-set-key [escape] 'acg/super-keyboard-quit)
;; rebind ESC functions to C-<escape>
(define-key key-translation-map (kbd "C-<escape>") (kbd "ESC"))
