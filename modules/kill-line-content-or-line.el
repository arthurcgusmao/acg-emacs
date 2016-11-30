(require 'crux)

(defun kill-line-content-or-line ()
  "Kill all characters in line and indent, or kill line
if there are only white spaces in it."
  (interactive)
  (if (current-line-empty-p)
      (kill-whole-line)
    (end-of-line)
    (crux-kill-line-backwards)))
    

;; helper function
(defun current-line-empty-p ()
  "Checks if line is empty (has only whitespace characters)"
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))
