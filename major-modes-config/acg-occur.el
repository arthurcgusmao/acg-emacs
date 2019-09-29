(defun occur-kill-line()
    "Quick and dirty discard line from occur resultset. (from
https://emacs.stackexchange.com/a/52865/13589)"
    (interactive)
    (read-only-mode -1)
    (kill-line)(delete-blank-lines)
    (read-only-mode +1))

;; switch to occur buffer automatically after running occur
(add-hook 'occur-hook (lambda () (switch-to-buffer-other-window "*Occur*")))

;; keybindings
(define-key occur-mode-map (kbd "d") 'occur-kill-line)
