(defun acg-delete-frame (event)
  "Kill normal buffers (non-emacs buffers) and delete the current
frame."
  (interactive "e")
  (acg-kill-normal-buffers)
  (delete-frame))

(defun acg-kill-normal-buffers ()
  "Kill all normal buffers (non-emacs buffers)."
  (dolist (cur (buffer-list))
    (if (not (equal (substring (buffer-name cur) 0 1) "*"))
        (kill-buffer cur))))

(define-key special-event-map [delete-frame] 'acg-delete-frame)
