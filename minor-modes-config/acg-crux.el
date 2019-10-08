(require 'crux)

;; Binding Crux commands
(crux-reopen-as-root-mode 1)
(global-set-key (kbd "<C-return>") 'crux-smart-open-line)
(global-set-key (kbd "<C-S-return>") 'crux-smart-open-line-above)
;; (global-set-key (kbd "<M-return>") 'indent-new-comment-line)
(global-set-key (kbd "<M-return>") 'open-line)
(global-set-key (kbd "C-/") (crux-with-region-or-line comment-or-uncomment-region))
(global-set-key (kbd "C-j") 'crux-top-join-line)
