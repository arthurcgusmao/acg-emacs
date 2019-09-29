;; Rebinding Emacs built-in commands
(global-set-key (kbd "C-s") 'save-buffer)

(acg-force-global-set-key "C-a" 'mark-whole-buffer)
(global-set-key (kbd "C-r") 'repeat)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-Z") 'redo)
(global-set-key (kbd "<M-return>") 'open-line)
(global-set-key (kbd "C-S-SPC") 'cycle-spacing)

(global-set-key (kbd "C-S-J") 'join-line)
(global-set-key (kbd "M-j") 'open-line)

;; for Windows only
(if (string-equal system-type "windows-nt")
    (and (global-set-key (kbd "<M-f4>") 'delete-frame)))
 


;; Todo List

;; - Ver questão cursor não mudar com scroll (prelude ui)
;; - Ver smart-mode-line


(provide 'acg-keybindings)
