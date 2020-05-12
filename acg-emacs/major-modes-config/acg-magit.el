(use-package magit)

;; Disable Magit asking to save files
(setq magit-save-repository-buffers nil)

;; Custom functions for visiting files

(defun acg/magit-diff-visit-file (file)
  "Same as `magit-diff-visit-file', but uses the default
`pop-to-buffer' (which in turn uses `display-buffer') to display
the respective file."
  (interactive (list (magit-file-at-point t t)))
  (magit-diff-visit-file--internal file nil #'pop-to-buffer))

(defun acg/magit-diff-display-file (file)
  "Display Magit's file at point in some buffer (uses the default
`display-buffer')"
  (interactive (list (magit-file-at-point t t)))
  (magit-diff-visit-file--internal file nil #'display-buffer))

(defun acg/magit-diff-display-file-and-next (file)
  "Same as `acg/magit-diff-display-file', but moves to the next
file before displaying file at point."
  (interactive (list (magit-file-at-point t t)))
  (magit-next-line)
  (magit-diff-visit-file--internal file nil #'display-buffer))

(defun acg/magit-diff-display-file-and-previous (file)
  "Same as `acg/magit-diff-display-file', but moves to the
previous file before displaying file at point."
  (interactive (list (magit-file-at-point t t)))
  (magit-previous-line)
  (magit-diff-visit-file--internal file nil #'display-buffer))


(with-eval-after-load 'magit
  (global-set-key (kbd "C-x g") 'magit-status)
  (define-key magit-status-mode-map (kbd "<C-tab>") nil)
  (define-key magit-status-mode-map (kbd "C-c <tab>") 'magit-section-cycle)
  (define-key magit-process-mode-map (kbd "<C-tab>") nil)
  (define-key magit-process-mode-map (kbd "C-c <tab>") 'magit-section-cycle)
  (define-key magit-log-mode-map (kbd "<C-tab>") nil)
  (define-key magit-log-mode-map (kbd "C-c <tab>") 'magit-section-cycle)

  (define-key magit-mode-map [escape] 'keyboard-quit)
  (define-key magit-log-mode-map [escape] 'magit-kill-this-buffer)
  (define-key magit-mode-map (kbd "C-w") 'magit-kill-this-buffer)
  (define-key magit-log-mode-map (kbd "C-w") 'magit-kill-this-buffer)
  (define-key magit-mode-map (kbd "q") 'magit-kill-this-buffer)
  (define-key magit-log-mode-map (kbd "q") 'magit-kill-this-buffer)

  (define-key with-editor-mode-map (kbd "C-w") 'with-editor-cancel)
  (define-key with-editor-mode-map (kbd "C-s") 'with-editor-finish)

  (define-key magit-mode-map (kbd "<C-up>") 'magit-section-backward-sibling)
  (define-key magit-mode-map (kbd "<C-down>") 'magit-section-forward-sibling)

  (mapc               ; Programatically remap M-[1-4] for all desired mode-maps
   (lambda (mode-map)
     (define-key (symbol-value mode-map) (kbd "M-1") nil)
     (define-key (symbol-value mode-map) (kbd "M-2") nil)
     (define-key (symbol-value mode-map) (kbd "M-3") nil)
     (define-key (symbol-value mode-map) (kbd "M-4") nil)
     (define-key (symbol-value mode-map) (kbd "C-c 1") 'magit-section-show-level-1-all)
     (define-key (symbol-value mode-map) (kbd "C-c 2") 'magit-section-show-level-2-all)
     (define-key (symbol-value mode-map) (kbd "C-c 3") 'magit-section-show-level-3-all)
     (define-key (symbol-value mode-map) (kbd "C-c 4") 'magit-section-show-level-4-all))
   '(magit-status-mode-map magit-diff-mode-map))

  (define-key magit-file-section-map (kbd "<return>") 'acg/magit-diff-visit-file)
  (define-key magit-file-section-map (kbd "<C-return>") 'acg/magit-diff-display-file)
  (define-key magit-file-section-map (kbd "<S-return>") 'acg/magit-diff-display-file-and-next)
  (define-key magit-file-section-map (kbd "<C-S-return>") 'acg/magit-diff-display-file-and-previous)
  )


(use-package transient)

(with-eval-after-load 'transient
  (define-key transient-map (kbd "<escape>") 'transient-quit-all)
  (define-key transient-map (kbd "<escape>") 'transient-quit-all)
  )


;; (global-set-key (kbd "C-x g") 'magit-status)

;; (define-key magit-popup-mode-map [escape] 'magit-popup-quit)
;; (define-key magit-mode-map [escape] 'magit-mode-bury-buffer)
;; (define-key magit-log-mode-map [escape] 'magit-log-bury-buffer)

;; (define-key magit-mode-map (kbd "C-w") 'magit-kill-this-buffer)



;; Provide commit message guidelines/feedback when committing;
;; taken from https://protesilaos.com/dotemacs

(use-package git-commit
  :after magit
  :custom
  (git-commit-fill-column 72)
  (git-commit-summary-max-length 50)
  (git-commit-known-pseudo-headers
   '("Signed-off-by"
     "Acked-by"
     "Modified-by"
     "Cc"
     "Suggested-by"
     "Reported-by"
     "Tested-by"
     "Reviewed-by"))
  (git-commit-style-convention-checks
   '(non-empty-second-line
     overlong-summary-line)))

;; Show word-granularity differences within diff hunks
(use-package magit-diff
  :straight nil
  :after magit
  :config
  (setq magit-diff-refine-hunk t))


;; Make Emacs work with ssh-ident
(use-package ssh-ident
  :straight (:host github :repo "arthurcgusmao/emacs-ssh-ident")
  :init (acg/add-to-env-path "~/.local/bin"))
