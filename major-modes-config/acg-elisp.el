
(defalias 'acg/eval-region-and-print
  (lambda (START END) (eval-region START END t))
  "Same as eval-region but prints output to the messages
  buffer.")

;; keybindings
(define-key emacs-lisp-mode-map (kbd "<f6>") 'eval-expression)
(define-key emacs-lisp-mode-map (kbd "<f7>") (crux-with-region-or-line acg/eval-region-and-print))
(define-key emacs-lisp-mode-map (kbd "<f8>") 'eval-buffer)

(define-key emacs-lisp-mode-map (kbd "C-c C-b") (acg/eval-with 'acg/eval-region-and-print 'mark-whole-buffer))
(define-key emacs-lisp-mode-map (kbd "C-c C-p") (acg/eval-with 'acg/eval-region-and-print 'mark-page))
(define-key emacs-lisp-mode-map (kbd "C-c C-c") (acg/eval-with 'acg/eval-region-and-print 'acg/mark-dwim))
(define-key emacs-lisp-mode-map (kbd "C-c C-l") (acg/eval-with 'acg/eval-region-and-print 'acg/expand-region-to-whole-lines))
