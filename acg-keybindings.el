;; Rebinding Emacs built-in commands
(global-set-key (kbd "C-s") 'save-buffer)
(define-key isearch-mode-map "\C-f" 'isearch-forward)
(define-key isearch-mode-map "\C-g" 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-G") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-S-V") 'isearch-yank-kill)
(global-set-key (kbd "M-w") nil)

(acg-force-global-set-key "C-a" 'mark-whole-buffer)
(global-set-key (kbd "C-d") 'bookmark-set)
(global-set-key (kbd "C-r") 'repeat)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-Z") 'redo)
(global-set-key (kbd "<M-return>") 'open-line)
(global-set-key (kbd "C-S-SPC") 'cycle-spacing)

(global-set-key (kbd "C-S-J") 'join-line)
(global-set-key (kbd "M-j") 'open-line)


;; Todo List

;; - Ver questão cursor não mudar com scroll (prelude ui)
;; - Ver smart-mode-line


(provide 'acg-keybindings)
