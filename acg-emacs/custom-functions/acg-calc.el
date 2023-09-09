(use-package calc
  :straight nil
  :init
  (defun acg/calc-eval ()
    "Takes expression in region and substitutes it for the evaluated value."
    (interactive)
    (unless (use-region-p)
      (acg/expand-region-to-whole-lines))
    (let ((selected-text
           (buffer-substring (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end))
      (insert (calc-eval selected-text))
      (define-key calc-mode-map (kbd "M-w") nil)))

  (defun acg/calc-paste-and-eval ()
    "Evaluates expression in the kill-ring and pastes the result"
    (interactive)
    (insert (calc-eval (car kill-ring))))

  :bind
  ("C-c c c" . calc)
  ("C-c c e" . acg/calc-eval)
  ("C-c c p" . acg/calc-paste-and-eval))
