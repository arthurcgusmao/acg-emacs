;; Editor configurations

(use-package emacs
  :straight nil
  :config
  (setq large-file-warning-threshold 100000000) ; Warn when opening files bigger than 100MB
  (setq select-enable-clipboard nil) ; Makes kill ring not mess with clipboard

  ;; Delete trailing whitespace every time before saving buffer
  (defun acg/toggle-delete-whitespace-on-save ()
    "Add/Removes `delete-trailing-whitespace' from `before-save-hook'."
    (interactive)
    (if (member #'delete-trailing-whitespace before-save-hook)
        (progn
          (remove-hook 'before-save-hook 'delete-trailing-whitespace)
          (message "Disabled delete trailing whitespace on save"))
      (add-hook 'before-save-hook 'delete-trailing-whitespace)
      (message "Enabled delete trailing whitespace on save")))
  ;; Start w/ delete whitespace on hook added
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (set-default 'truncate-lines t)    ; Disable truncation of line
  (setq require-final-newline t)     ; Always save file with newline at end
  ;; Consider hifened words as a single word
  (global-superword-mode 1)
  ;; Enable advanced features (that are by default disabled)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  ;; automatically run fill-paragraph
  ;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

  ;; fill-paragraph function only leave one space after period instead of two
  (setq sentence-end-double-space nil)

  ;; make scratch buffer always start as text-mode and not lisp-mode
  (setq initial-major-mode 'text-mode)

  ;; visual line mode
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
  ;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

  ;; delete the selection with a keypress
  (delete-selection-mode t)

  ;; Remove annoying ring bell
  (setq ring-bell-function 'ignore)


  ;; Avoid performance issues in files with very long lines.

  ;; Emacs supports text in scripts whose ordering of display is from right to
  ;; left; however, digits and Latin text in these scripts are still displayed
  ;; left to right. This feature adds to the amount of line scans which can
  ;; cause Emacs to hang; disabling it when not using such scripts can increase
  ;; performance.
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)
  ;; Automatically try to improve performance for files with long lines.
  (global-so-long-mode 1)


  ;; MS Windows configs
  (if (string-equal system-type "windows-nt")
      (progn
        ;; disables the annoying ring bell when something goes wrong
        (setq-default visible-bell t)
        ;; enable the windows key to be processed by Emacs (instead of passed to Windows directly)
        (setq w32-pass-lwindow-to-system nil
              w32-lwindow-modifier 'super)
        ;; default to unix coding system
        (setq-default buffer-file-coding-system 'utf-8-unix)))

  ;; Kill minibuffer when it loses focus
  (defun stop-using-minibuffer ()
    "kill the minibuffer"
    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
      (abort-recursive-edit)))
  (add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)
  (setq enable-recursive-minibuffers t)
  )


;; tabs, indent, spacing

(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))

;; use only spaces (and not tabs) for whitespace
(setq-default indent-tabs-mode nil)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(define-key indent-rigidly-map (kbd "<right>") 'indent-rigidly-right-to-tab-stop)
(define-key indent-rigidly-map (kbd "<left>") 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map (kbd "<S-right>") 'indent-rigidly-right)
(define-key indent-rigidly-map (kbd "<S-left>") 'indent-rigidly-left)


;; remote access (TRAMP)
(use-package tramp
  :straight nil
  :config
  (setq tramp-persistency-file-name (concat acg/history-dir "tramp"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;;; Make TRAMP faster
  ;; Make VC ignore remote files
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  ;; Decrease verbosity of TRAMP logs
  (setq tramp-verbose 1))


;; Record various types of history
;; Adapted from https://protesilaos.com/dotemacs/#h:2733674b-51f9-494e-b34d-e8842ac4ef96

;; Keep a list of recently opened files
(use-package recentf
  :straight nil
  :config
  (setq recentf-save-file (concat acg/history-dir "recentf"))
  (setq recentf-max-saved-items 300)
  (setq recentf-max-menu-items 10)
  :hook ((after-init . recentf-mode)))

;; Remember minibuffer history for better suggestions
(use-package savehist
  :straight nil
  :config
  (setq savehist-file (concat acg/history-dir "savehist"))
  (setq history-length 2000)
  (setq history-delete-duplicates nil)
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

;; Remember where point was last time you visited a file
(use-package saveplace
  :straight nil
  :config
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

;; Backup and Autosave of files
(use-package emacs
  :straight nil
  :config
  ;; Adapted from https://stackoverflow.com/a/18330742/5103881
  (setq backup-directory-alist `(("." . ,acg/file-backup-dir))
        make-backup-files t               ; backup of a file the first time it is saved.
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
  ;; Disable autosave of sensitive data
  (setq auto-mode-alist
        (append
         (list
          '("\\.\\(vcf\\|gpg\\)$" . sensitive-minor-mode)
          )
         auto-mode-alist))

  ;; Save the last session (for when you open emacs the next time)
  (desktop-save-mode 0)
  (setq desktop-dirname (concat user-emacs-directory "desktop/")
        desktop-path (list desktop-dirname)))

;; Navigate window configuration history
(use-package winner
  :straight nil
  :hook (after-init-hook . winner-mode))


;; Rebinding Emacs built-in commands
(use-package emacs
  :straight nil
  :config
  (acg/force-global-set-key "C-a" 'mark-whole-buffer)

  ;; Rebind C-[ and then use [control-bracketleft] to rebind in other places
  (define-key input-decode-map (kbd "C-[") [control-bracketleft])

  ;; for MS Windows only
  (if (string-equal system-type "windows-nt")
      (and (global-set-key (kbd "<M-f4>") 'delete-frame)))

  (defun acg/widen-recenter ()
    "Same as `widen' but run `recenter' afterwards."
    (interactive)
    (widen)
    (recenter))

  :bind
  ("C-l" . recenter)
  ("C-r" . repeat)
  ("C-z" . undo)
  ("C-S-Z" . redo)
  ("C-S-SPC" . cycle-spacing)
  ("C-S-J" . join-line)
  ("C-x n w" . acg/widen-recenter))


(use-package auth-source
  :straight nil
  :config
  ;; Never ask to save authentication credentials in a file
  (setq auth-source-save-behavior nil))
