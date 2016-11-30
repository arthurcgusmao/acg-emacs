;; Editor Configurations


;; disable truncation of line
(set-default 'truncate-lines t)

;; makes kill ring not mess with clipboard
(setq x-select-enable-clipboard nil)

;; consider camelCased as 2 words
(global-subword-mode 1)

;; enable disabled advanced features
(put 'downcase-region 'disabled nil)

    
;;;;;;;;;;;;;;;;;;;;;
;; search/replace
;;;;;;;;;;;;;;;;;;;;;

;; anzu-mode enhances isearch & query-replace by showing
;; total matches and current match position
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

;; delete the selection with a keypress
(delete-selection-mode t)


;;;;;;;;;;;;;;;;;;;;;
;; undo
;;;;;;;;;;;;;;;;;;;;;

;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; lets you undo and redo changes in the window configuration
(winner-mode 1)


;;;;;;;;;;;;;;;;;;;;;
;; files, buffers, backup, autosave
;;;;;;;;;;;;;;;;;;;;;

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,(concat acg-backup-dir "files-autosave-and-backup/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat acg-backup-dir "files-autosave-and-backup/") t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; saving the last session (for when you open emacs the next time)
(desktop-save-mode 0)


;;;;;;;;;;;;;;;;;;;;;
;; tabs and indent
;;;;;;;;;;;;;;;;;;;;;

(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))

;; use only spaces (and not tabs) for whitespace
(setq-default indent-tabs-mode nil)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)



(provide 'acg-editor)
