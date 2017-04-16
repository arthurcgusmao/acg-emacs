;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; keybindings
(acg-force-global-set-key "M-z" 'undo-tree-visualize)
(define-key undo-tree-visualizer-mode-map (kbd "RET") 'undo-tree-visualizer-quit)
(define-key undo-tree-visualizer-mode-map [escape] 'undo-tree-visualizer-quit)
