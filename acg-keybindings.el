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


;; tab and indent
(global-set-key (kbd "C-i") (crux-with-region-or-line indent-region))
(global-set-key (kbd "C-S-I") 'indent-region-or-buffer)

;; changing windows
(global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-!") 'delete-window)
(global-set-key (kbd "C-2") 'split-window-right)
(global-set-key (kbd "C-@") 'split-window-below)

;; changing buffers
(global-set-key (kbd "M-q") 'crux-switch-to-previous-buffer)
(global-set-key (kbd "C-w") 'kill-this-buffer)



;; Binding Crux commands
(crux-reopen-as-root-mode 1)
(global-set-key (kbd "<C-return>") 'crux-smart-open-line)
(global-set-key (kbd "<C-S-return>") 'crux-smart-open-line-above)
(global-unset-key (kbd "C-/"))
(local-unset-key (kbd "C-/"))
(define-key undo-tree-map (kbd "C-/") nil)
(global-set-key (kbd "C-/") (crux-with-region-or-line comment-or-uncomment-region))
(global-set-key (kbd "C-S-C") (crux-with-region-or-line clipboard-kill-ring-save))
(global-set-key (kbd "C-S-X") (crux-with-region-or-line clipboard-kill-region))
(global-set-key (kbd "C-j") 'crux-top-join-line)
(global-set-key (kbd "C-S-J") 'join-line)


;; Binding Custom Functions commands
(global-set-key (kbd "<home>") 'prelude-move-beginning-of-line)
(global-set-key (kbd "C-c t") 'xah-open-in-terminal)
(global-set-key (kbd "C-c o") 'xah-open-in-desktop)
(global-set-key (kbd "C-c SPC") 'just-one-space)
(global-set-key (kbd "<S-return>") 'newline-above)
(global-unset-key (kbd "C-n"))
(global-set-key (kbd "C-n") 'custom-scratch-buffer-create)
(global-set-key (kbd "C-S-V") 'clipboard-paste-replace-selection)
(global-set-key (kbd "C-;") 'append-or-remove-semicolon-to-eol)
(global-set-key (kbd "C-,") 'append-or-remove-comma-to-eol)

;;; esc always quits
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'my-super-keyboard-quit)
;; rebind ESC functions to C-<escape>
(define-key key-translation-map (kbd "C-<escape>") (kbd "ESC"))



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
