(require 'crux)

(defun acg-clipboard-paste-replace-selection ()
  "Paste text from clipboard overwriting the current selection"
  (interactive)
  (if (use-region-p)
      (let ((clipboard-paste-region-beginning (region-beginning)))
        (delete-region (region-beginning) (region-end))
        (x-clipboard-yank)
        (indent-region clipboard-paste-region-beginning (region-end))
        )
    (let ((clipboard-paste-point-start (point)))
      (x-clipboard-yank)
      (indent-region clipboard-paste-point-start (point))
      ))
  (message "Text pasted and indented."))


;;keybindings
(global-set-key (kbd "C-S-C") (crux-with-region-or-line clipboard-kill-ring-save))
(global-set-key (kbd "C-S-X") (crux-with-region-or-line clipboard-kill-region))
(global-set-key (kbd "C-S-V") 'acg-clipboard-paste-replace-selection)

