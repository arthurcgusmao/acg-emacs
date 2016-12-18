(defun my-custom-latex-initialization ()
  (define-key latex-mode-map (kbd "C-j") nil)
  (define-key latex-mode-map (kbd "C-c C-j") 'tex-terminate-paragraph)
  (define-key latex-mode-map (kbd "<C-return>") nil)
  (define-key latex-mode-map (kbd "C-c <C-return>") 'tex-feed-input)
  )

(eval-after-load "tex-mode" '(my-custom-latex-initialization))
