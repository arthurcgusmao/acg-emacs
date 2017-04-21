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


;; from https://emacs.stackexchange.com/questions/3330/how-to-reopen-just-killed-buffer-like-c-s-t-in-firefox-browser
;; making C-S-T reopen last closed buffer as in chrome

(defvar acg-killed-file-list nil
  "List of recently killed files.")

(defun acg-add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name acg-killed-file-list)))

(add-hook 'kill-buffer-hook #'acg-add-file-to-killed-file-list)

(defun acg-reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when acg-killed-file-list
    (find-file (pop acg-killed-file-list))))


;; keybindings

(global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "M-q") 'crux-switch-to-previous-buffer)

(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-!") 'delete-window)
(global-set-key (kbd "C-2") 'acg-split-window-right)
(global-set-key (kbd "C-@") 'acg-split-window-below)

(global-set-key (kbd "C-q") 'delete-window)

(eval-after-load "calc" '(define-key calc-mode-map (kbd "C-w") nil))
(global-set-key (kbd "C-w") 'acg-kill-buffer-and-window)

(global-set-key (kbd "C-S-T") 'acg-reopen-killed-file)
