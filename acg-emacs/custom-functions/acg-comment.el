(defun acg/comment-line (&optional n)
  "Same as `comment-line' but does not move cursor position."
  (interactive "p")
  (save-excursion
    (comment-line n)))

(advice-add 'acg/comment-line :after #'acg/with-mark-active)

(global-set-key (kbd "C-/") 'acg/comment-line)
(global-set-key (kbd "C-?") 'comment-dwim)
