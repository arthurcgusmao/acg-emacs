;; Rebinding Emacs built-in commands
(acg/force-global-set-key "C-a" 'mark-whole-buffer)
(global-set-key (kbd "C-r") 'repeat)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-Z") 'redo)
(global-set-key (kbd "C-S-SPC") 'cycle-spacing)
(global-set-key (kbd "C-S-J") 'join-line)

;; for MS Windows only
(if (string-equal system-type "windows-nt")
    (and (global-set-key (kbd "<M-f4>") 'delete-frame)))


(provide 'acg-keybindings)
