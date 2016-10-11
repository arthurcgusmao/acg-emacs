;; Editor Configurations


;; disable truncation of line
(set-default 'truncate-lines t)

;; makes kill ring not mess with clipboard
(setq x-select-enable-clipboard nil)

;; consider camelCased as 2 words
(global-subword-mode 1)

    
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
;; backup and autosave
;;;;;;;;;;;;;;;;;;;;;

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; saving the last session (for when you open emacs the next time)
(desktop-save-mode 0)


;;;;;;;;;;;;;;;;;;;;;
;; scrolling
;;;;;;;;;;;;;;;;;;;;;

;; restore the cursor to position when scrolling through the page
(require 'scroll-restore)
(scroll-restore-mode 1)

;; always keep some lines at bottom/top when scroll with keypad
(require 'smooth-scrolling)
(setq smooth-scrolling-mode 1)

;; scroll wheel move one line per scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
;; scroll speed always steady
(setq mouse-wheel-progressive-speed nil)


;; enable disabled advanced features
(put 'downcase-region 'disabled nil)


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
