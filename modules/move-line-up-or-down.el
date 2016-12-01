(defun move-line-down ()
  "Moves current line down."
  (interactive)
  (let ((col (current-column)))
    (forward-line)
    (transpose-lines 1)
    (previous-line)
    (move-to-column col)))

(defun move-line-up ()
  "Moves current line up."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (previous-line 2)
    (move-to-column col)))

;; binding
(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)
