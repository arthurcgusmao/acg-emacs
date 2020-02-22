;; sensible undo
(global-undo-tree-mode)

;; autosave the undo-tree history
(setq undo-tree-auto-save-history nil)
;; (setq undo-tree-history-directory-alist `((".*" . ,(expand-file-name (concat user-emacs-directory "undo-tree-history/")))))

;; keybindings
(acg/force-global-set-key "M-z" 'undo-tree-visualize)
(define-key undo-tree-map (kbd "C-/") nil)
(define-key undo-tree-map (kbd "C-?") nil)
(define-key undo-tree-visualizer-mode-map (kbd "RET") 'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map [escape] 'undo-tree-visualizer-quit)
