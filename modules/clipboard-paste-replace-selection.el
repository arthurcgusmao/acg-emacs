(defun clipboard-paste-replace-selection ()
  "Paste text from clipboard overwriting the current selection"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (x-clipboard-yank))
