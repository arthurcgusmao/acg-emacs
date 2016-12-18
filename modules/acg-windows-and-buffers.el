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


;; keybindings

(global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "M-q") 'crux-switch-to-previous-buffer)

(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-!") 'delete-window)
(global-set-key (kbd "C-2") 'acg-split-window-right)
(global-set-key (kbd "C-@") 'acg-split-window-below)

(global-set-key (kbd "C-w") 'kill-buffer-and-window)
(global-set-key (kbd "C-q") 'delete-window)

