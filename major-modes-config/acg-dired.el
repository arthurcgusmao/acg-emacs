(require 'dired-subtree)

(define-key dired-mode-map (kbd "C-o") nil)
(define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)
(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-cycle)
(define-key dired-mode-map (kbd "<S-iso-lefttab>") 'dired-subtree-remove)
