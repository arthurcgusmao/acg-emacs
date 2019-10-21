(require 'magit)

;; disable Emacs's built-in version control since not using it
(setq vc-handled-backends nil)


(with-eval-after-load 'magit
  (global-set-key (kbd "C-x g") 'magit-status)
  ;; (define-key magit-popup-mode-map [escape] 'magit-popup-quit)
  (define-key magit-mode-map ["C-h"] nil)
  (define-key magit-log-mode-map ["C-h"] nil)

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
  )


(require 'transient)

(with-eval-after-load 'transient
  (define-key transient-map (kbd "<escape>") 'transient-quit-all)
  (define-key transient-map (kbd "<escape>") 'transient-quit-all)
  )


;; (global-set-key (kbd "C-x g") 'magit-status)

;; (define-key magit-popup-mode-map [escape] 'magit-popup-quit)
;; (define-key magit-mode-map [escape] 'magit-mode-bury-buffer)
;; (define-key magit-log-mode-map [escape] 'magit-log-bury-buffer)

;; (define-key magit-mode-map (kbd "C-w") 'magit-kill-this-buffer)



;; Follow guidelines when committing;
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
