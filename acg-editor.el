;; editor configurations

;; disable truncation of line
(set-default 'truncate-lines t)

;; makes kill ring not mess with clipboard
(setq x-select-enable-clipboard nil)

;; consider hifened words as a single word
(global-superword-mode 1)

;; enable disabled advanced features
(put 'downcase-region 'disabled nil)

;; lets you undo and redo changes in the window configuration
(winner-mode 1)

;; automatically run fill-paragraph
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

;; fill-paragraph function only leave one space after period instead of two
(setq sentence-end-double-space nil)

;; make scratch buffer always start as text-mode and not lisp-mode
(setq initial-major-mode 'text-mode)

;; visual line mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))


;; search/replace

;; anzu-mode enhances isearch & query-replace by showing
;; total matches and current match position
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

;; delete the selection with a keypress
(delete-selection-mode t)


;; files, buffers, backup, autosave

;; from https://stackoverflow.com/a/18330742/5103881
(setq backup-directory-alist `(("." . ,acg-backup-dir)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
;; disable autosave of sensitive data
(setq auto-mode-alist
      (append
       (list
        '("\\.\\(vcf\\|gpg\\)$" . sensitive-minor-mode)
        )
       auto-mode-alist))

;; saving the last session (for when you open emacs the next time)
(desktop-save-mode 0)
(setq desktop-dirname (concat default-emacs-dir "desktop/")
      desktop-path (list desktop-dirname))

;; keep a list of recently opened files
(setq-default recent-save-file (concat default-emacs-dir "recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 300)
(setq recentf-max-menu-items 300)


;; tabs, indent, spacing

(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))

;; use only spaces (and not tabs) for whitespace
(setq-default indent-tabs-mode nil)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)


(provide 'acg-editor)
