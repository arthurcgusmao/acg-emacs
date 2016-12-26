(require 'crux)

;; Rebinding Emacs built-in commands
(global-set-key (kbd "C-s") 'save-buffer)
(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map "\C-f" 'isearch-forward)
(define-key isearch-mode-map "\C-g" 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-G") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-S-V") 'isearch-yank-kill)
(global-set-key (kbd "C-S-F") 'isearch-forward-symbol-at-point)

(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "<M-return>") 'open-line)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-Z") 'redo)
(global-set-key (kbd "C-r") 'repeat)
(global-set-key (kbd "C-c SPC") 'just-one-space)
(global-set-key (kbd "C-d") 'bookmark-set)


;; Binding Crux commands
(crux-reopen-as-root-mode 1)
(global-set-key (kbd "<C-return>") 'crux-smart-open-line)
(global-set-key (kbd "<C-S-return>") 'crux-smart-open-line-above)
(global-set-key (kbd "<M-return>") 'indent-new-comment-line)
(define-key undo-tree-map (kbd "C-/") nil)
(global-set-key (kbd "C-/") (crux-with-region-or-line comment-or-uncomment-region))
(global-set-key (kbd "C-j") 'crux-top-join-line)
(global-set-key (kbd "C-S-J") 'join-line)
(global-set-key (kbd "M-j") 'open-line)


;; Binding "Visual" C-v keys
(global-unset-key (kbd "C-v"))
(global-set-key (kbd "C-v h h") 'highlight-symbol-at-point)
(global-set-key (kbd "C-v h r") 'highlight-regexp)
(global-set-key (kbd "C-v h p") 'highlight-phrase)
(global-set-key (kbd "C-v h u") 'unhighlight-regexp)





;; ----------------------------------------------------------------------
;; Todo List
;;
;; - Ver questão cursor não mudar com scroll (prelude ui)
;; - Ver smart-mode-line




(provide 'acg-keybindings)
