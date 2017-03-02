(defun acg-calc-eval ()
  "Takes expression in region and substitutes it for the evaluated value."
  (interactive)
  (if (use-region-p)
      (let ((selected-text
             (buffer-substring (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert (calc-eval selected-text)))))
