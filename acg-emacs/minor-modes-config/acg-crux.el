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

  (defun acg/smart-open-line-below ()
    "Alternative to `crux-smart-open-line', since the original
does not indent according to
`electric-indent-post-self-insert-function'."
    (interactive)
    (move-end-of-line nil)
    (insert "\n")
    (forward-line -1)
    (if electric-indent-inhibit
        ;; We can't use `indent-according-to-mode' in languages like Python,
        ;; as there are multiple possible indentations with different meanings.

        ;; If this new line follows a blank line (current line), it should be
        ;; indented with its same characters. Else, it should be indented
        ;; according to the mode (default).
        (if (acg/line-empty-p)
            (let* ((indent-end (progn (move-to-mode-line-start) (point)))
                   (indent-start (progn (move-beginning-of-line nil) (point)))
                   (indent-chars (buffer-substring indent-start indent-end)))
              (forward-line 1)
              (insert indent-chars))
          (forward-line 1)
          (indent-according-to-mode))
      ;; Just use the current major-mode's indent facility.
      (forward-line 1)
      (indent-according-to-mode)))

  :bind
  (("<C-return>" . acg/smart-open-line-below)
   ("<C-S-return>" . crux-smart-open-line-above)
   ;; ("<M-return>" . indent-new-comment-line)
   ("<M-return>" . acg/open-line)
   ("C-j" . crux-top-join-line)))
