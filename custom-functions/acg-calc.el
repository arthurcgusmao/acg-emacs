(defun acg/calc-eval ()
  "Takes expression in region and substitutes it for the evaluated value."
  (interactive)
  (if (use-region-p)
      (let ((selected-text
             (buffer-substring (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert (calc-eval selected-text))
        (define-key calc-mode-map (kbd "C-w") nil))))

(defun acg/calc-paste-and-eval ()
  "Evaluates expression in the kill-ring and pastes the result"
  (interactive)
  (insert (calc-eval (car kill-ring))))

(global-set-key (kbd "C-c c c") 'calc)
(global-set-key (kbd "C-c c e") 'acg/calc-eval)
(global-set-key (kbd "C-c c p") 'acg/calc-paste-and-eval)
