(use-package replace
  :straight nil
  :config

  ;; Switch to occur buffer automatically after running occur
  (add-hook 'occur-hook (lambda () (switch-to-buffer-other-window "*Occur*")))


  ;; Custom functions

  (defun acg/occur-kill-line ()
    "Quick and dirty discard line from occur resultset.
From https://emacs.stackexchange.com/a/52865/13589"
    (interactive)
    (let ((inhibit-read-only t))
      (kill-whole-line)
      (delete-blank-lines)))

  (defun acg/occur-mode-display-occurrence-and-next ()
    "Same as `occur-mode-display-occurrence' but jumps to next
line afterwards."
    (interactive)
    (occur-mode-display-occurrence)
    (next-line))

  (defun acg/occur-mode-display-occurrence-and-previous ()
    "Same as `occur-mode-display-occurrence' but jumps to previous
line afterwards."
    (interactive)
    (occur-mode-display-occurrence)
    (previous-line))


  :bind
  (:map occur-mode-map
        ("d" . acg/occur-kill-line)
        ("<C-return>" . occur-mode-display-occurrence)
        ("<S-return>" . acg/occur-mode-display-occurrence-and-next)
        ("<C-S-return>" . acg/occur-mode-display-occurrence-and-previous)
        ("C-o" . nil)))
