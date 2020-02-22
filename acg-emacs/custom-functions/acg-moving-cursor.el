(defun acg/move-beginning-of-line (arg)
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

(defun acg/move-beginning-of-visual-line (arg)
  "Same as `acg/move-beginning-of-line' but for the visual line."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (beginning-of-visual-line 1))))

;; keybindings
(global-set-key (kbd "<home>") 'acg/move-beginning-of-visual-line)
(global-set-key (kbd "<C-M-left>") 'acg/move-beginning-of-visual-line)
(global-set-key (kbd "<C-M-right>") 'move-end-of-line)
(global-set-key (kbd "<s-right>") (acg/with-subword-mode #'forward-word))
(global-set-key (kbd "<s-left>") (acg/with-subword-mode #'backward-word))
