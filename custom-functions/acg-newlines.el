(require 'crux)

(defun acg/newline-above ()
  "Insert a newline above with the rest of current line."
  (interactive)
  (kill-line)
  (crux-smart-open-line-above)
  (yank)
  (back-to-indentation))

;; keybindings
(global-set-key (kbd "<S-return>") 'acg/newline-above)
