;; -------------------------------------------------------------------------
;; Editor Configurations



;; saving the last session (for when you open emacs the next time)
(desktop-save-mode 1)

;; enabling auto-complete mode
(auto-complete-mode t) ;; not working -- don't know why

;; lets you undo and redo changes in the window configuration
(winner-mode 1)

;; disable truncation of line
(set-default 'truncate-lines t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; makes kill ring not mess with clipboard
(setq x-select-enable-clipboard nil)


;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)


;; Make emacs identify camelCased words
(subword-mode 1)


;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; projectile is a project management mode
(require 'projectile)
;;(setq projectile-cache-file (expand-file-name  "projectile.cache" prelude-savefile-dir))
(projectile-global-mode t)

;; anzu-mode enhances isearch & query-replace by showing total matches and current match position
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)


;; restore the cursor to position when scrolling through the page
(require 'scroll-restore)
(scroll-restore-mode 1)

;; smooth scrolling mode makes it always keep some lines at bottom/top when you scroll with the keypad
(require 'smooth-scrolling)
(setq smooth-scrolling-mode 1)

;; makes the scroll wheel one move one line per scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
;; makes the scroll speed always steady
(setq mouse-wheel-progressive-speed nil)






(provide 'acg-editor)
