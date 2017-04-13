;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; keybindings
(global-set-key (kbd "s-z") 'undo-tree-visualize)
