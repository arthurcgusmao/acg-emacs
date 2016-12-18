(defun append-or-remove-char-to-eol (char-code char)
  "Add/removes specific char at the end of the line and return to current position"
  (setq current (point))
  (end-of-line)
  (if (not (= (preceding-char) char-code))
    (insert char)
    (delete-backward-char 1))
  (goto-char current))

(defun append-or-remove-semicolon-to-eol ()
  "Add/removes semicolon to the end of the line"
  (interactive)
  (append-or-remove-char-to-eol 59 ";"))

(defun append-or-remove-comma-to-eol ()
  "Add/removes comma to the end of the line"
  (interactive)
  (append-or-remove-char-to-eol 44 ","))

(global-set-key (kbd "C-;") 'append-or-remove-semicolon-to-eol)
(global-set-key (kbd "C-,") 'append-or-remove-comma-to-eol)
