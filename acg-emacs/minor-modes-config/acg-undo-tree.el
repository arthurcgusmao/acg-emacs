(use-package undo-tree
  :config
  ;; Sensible undo
  (global-undo-tree-mode)

  ;; Autosave the undo-tree history
  (setq undo-tree-auto-save-history nil)
  ;; (setq undo-tree-history-directory-alist `((".*" . ,(expand-file-name (concat user-emacs-directory "undo-tree-history/")))))

  ;; Custom commands to preserve active region when undoing
  (defun acg/undo-tree-undo (&optional arg)
    "Same as `undo-tree-undo' but does not deactivate selected
region nor moves the cursor position."
    (interactive "*P")
    (save-excursion
      (undo-tree-undo arg)
      (setq deactivate-mark nil)))

  (defun acg/undo-tree-redo (&optional arg)
    "Same as `undo-tree-redo' but does not deactivate selected
region nor moves the cursor position."
    (interactive "*P")
    (save-excursion
      (undo-tree-redo arg)
      (setq deactivate-mark nil)))

  ;; Keybindings
  (acg/force-global-set-key "M-z" 'undo-tree-visualize)
  :bind
  (:map undo-tree-map
   ("C-/" . nil)
   ("C-?" . nil)
   ("C-z" . undo-tree-undo)
   ("C-S-z" . undo-tree-redo)
   :map undo-tree-visualizer-mode-map
   ("RET" . undo-tree-visualizer-quit)
   ([escape] . undo-tree-visualizer-abort)))
