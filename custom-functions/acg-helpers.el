;; only helper functions in this buffer.

(defun acg-current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun acg-current-indentation-column-p ()
  (save-excursion
    (back-to-indentation)
    (current-column)))
