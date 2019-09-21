(defun acg-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line. If
point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line. If ARG is not nil or 1, move forward
ARG - 1 lines first. If point reaches the beginning or end of the
buffer, stop there. (function taken from prelude)"
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


(defun acg-get-word-mode ()
  (cond
   (superword-mode 'superword-mode)
   (subword-mode 'subword-mode)))

(defun acg-reset-word-mode (original-word-mode altered-word-mode)
  (unless (eq original-word-mode altered-word-mode)
    (if original-word-mode
        (set original-word-mode 1)
      (set altered-word-mode 0))))

(defun acg-forward-subword (&optional arg)
  "Similar to `forward-word' when in subword-mode, but does not
  depend on subword-mode activation."
  (interactive "^p")
  (setq arg (or arg 1))
  (with-current-buffer (current-buffer)
    (let ((word-mode (acg-get-word-mode)))
      (subword-mode 1)
      (forward-word arg)
      (acg-reset-word-mode word-mode 'subword-mode))))

(defun acg-backward-subword (&optional arg)
  "Similar to `forward-word' when in subword-mode, but does not
  depend on subword-mode activation."
  (interactive "^p")
  (setq arg (or arg 1))
  (with-current-buffer (current-buffer)
    (let ((word-mode (acg-get-word-mode)))
      (subword-mode 1)
      (backward-word arg)
      (acg-reset-word-mode word-mode 'subword-mode))))


;; keybindings
(global-set-key (kbd "<home>") 'acg-move-beginning-of-line)
(global-set-key (kbd "<C-M-left>") 'acg-move-beginning-of-line)
(global-set-key (kbd "<C-M-right>") 'move-end-of-line)
(global-set-key (kbd "<s-right>") 'acg-forward-subword)
(global-set-key (kbd "<s-left>") 'acg-backward-subword)
