(defun occur-kill-line()
    "Quick and dirty discard line from occur resultset. (from
https://emacs.stackexchange.com/a/52865/13589)"
    (interactive)
    (read-only-mode -1)
    (kill-line)(delete-blank-lines)
    (read-only-mode +1))

(define-key occur-mode-map (kbd "k") 'occur-kill-line)
