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



(defun acg-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2017-04-19"
  (interactive)
  (let (
        (deactivate-mark nil)
        -p1 -p2)
    (if (use-region-p)
        (setq -p1 (region-beginning)
              -p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]-_")
        (setq -p1 (point))
        (skip-chars-forward "[:alnum:]-_")
        (setq -p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region -p1 -p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region -p1 -p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region -p1 -p2)
      (put this-command 'state 0)))))

(global-set-key (kbd "M-c") 'acg-toggle-letter-case)
