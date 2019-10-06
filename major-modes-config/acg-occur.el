(defun occur-kill-line()
  "Quick and dirty discard line from occur resultset. (from
https://emacs.stackexchange.com/a/52865/13589)"
  (interactive)
  (let ((buffer-read-only nil))
    (kill-whole-line)
    (delete-blank-lines)))

;; switch to occur buffer automatically after running occur
(add-hook 'occur-hook (lambda () (switch-to-buffer-other-window "*Occur*")))

;; keybindings
(define-key occur-mode-map (kbd "d") 'occur-kill-line)
