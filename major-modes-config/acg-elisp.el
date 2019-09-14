
(defalias 'eval-region-and-print
  (lambda (START END) (eval-region START END t))
  "Same as eval-region but prints output to the messages
  buffer.")

;; keybindings
(define-key emacs-lisp-mode-map (kbd "<f6>") 'eval-expression)
(define-key emacs-lisp-mode-map (kbd "<f7>") (crux-with-region-or-line eval-region-and-print))
(define-key emacs-lisp-mode-map (kbd "<f8>") 'eval-buffer)
