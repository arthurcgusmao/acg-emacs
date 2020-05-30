(use-package crux
  :config
  (crux-reopen-as-root-mode 1)

  (defun acg/open-line ()
    "Alternative version of `open-line' that indents the newly
created line."
    (interactive)
    ;; @todo: add wrapper to handle execution as single "undo"
    (newline)
    (newline)
    (previous-line)
    (indent-according-to-mode)
    (delete-char 1)
    (previous-line)
    (end-of-line))

  :bind
  (("<C-return>" . crux-smart-open-line)
   ("<C-S-return>" . crux-smart-open-line-above)
   ;; ("<M-return>" . indent-new-comment-line)
   ("<M-return>" . acg/open-line)
   ("C-j" . crux-top-join-line)))
