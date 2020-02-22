(use-package crux
  :config
  (crux-reopen-as-root-mode 1)
  :bind
  (("<C-return>" . crux-smart-open-line)
   ("<C-S-return>" . crux-smart-open-line-above)
   ;; ("<M-return>" . indent-new-comment-line)
   ("<M-return>" . open-line)
   ("C-j" . crux-top-join-line)))
