(require 'crux)

(defun acg-split-window-right ()
  "Same as `split-window-right' but runs `other-window' afterwards."
  (interactive)
  (split-window-right)
  (other-window 1)
  (other-buffer))

(defun acg-split-window-below ()
  "Same as `split-window-below' but runs `other-window' afterwards."
  (interactive)
  (split-window-below)
  (other-window 1)
  (other-buffer))


(defun acg-kill-buffer-and-window ()
  "Same as `kill-buffer-and-window' but acts differently when in
*Messages* buffer."
  (interactive)
  (if (equal (buffer-name (current-buffer)) "*Messages*")
      (next-buffer)
    (kill-buffer-and-window)))


;; keybindings

(global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "M-q") 'crux-switch-to-previous-buffer)

(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-!") 'delete-window)
(global-set-key (kbd "C-2") 'acg-split-window-right)
(global-set-key (kbd "C-@") 'acg-split-window-below)

(global-set-key (kbd "C-w") 'acg-kill-buffer-and-window)
(global-set-key (kbd "C-q") 'delete-window)

