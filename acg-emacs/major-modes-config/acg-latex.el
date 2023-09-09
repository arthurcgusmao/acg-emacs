(defun acg/latex-initialization ()
  (define-key latex-mode-map (kbd "M-j") nil)
  (define-key latex-mode-map (kbd "C-c C-j") 'tex-terminate-paragraph)
  (define-key latex-mode-map (kbd "<C-return>") nil)
  (define-key latex-mode-map (kbd "C-c <C-return>") 'tex-feed-input)

  (define-key latex-mode-map (kbd "<f5>") 'preview-section)
  (define-key latex-mode-map (kbd "<f6>") 'preview-at-point)
  )
(eval-after-load "tex-mode" '(acg/latex-initialization))

;; bigger preview image
(set-default 'preview-scale-function 1.5)
