(defun delete-between-pair (char)
  "Delete in between the given pair"
  (interactive "cDelete between char: ")
  (seek-backward-to-char char)
  (forward-char 1)
  (zap-to-char 1 char)
  (insert char)
  (forward-char -1))
