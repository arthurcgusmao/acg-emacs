(use-package comint
  :straight nil
  :config
  (setq-default comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
  (setq-default comint-scroll-to-bottom-on-output t) ; always add output at the bottom
  (setq-default comint-scroll-show-maximum-output t) ; scroll to show max possible output
  (setq-default comint-completion-autolist t)        ; show completion list when ambiguous
  (setq-default comint-input-ignoredups t)           ; no duplicates in command history
  (setq-default comint-completion-addsuffix t)       ; insert space/slash after file completion
  ;; Interpret and use ansi color codes in shell output windows
  (ansi-color-for-comint-mode-on)
 )


;; keybindings
(add-hook 'comint-mode-hook
  	  (function (lambda ()
                      (acg/local-set-minor-mode-key 'company-mode-map (kbd "<tab>") 'completion-at-point)
                      (local-set-key (kbd "<tab>") 'completion-at-point)

                      (local-set-key (kbd "<up>") 'comint-previous-input)
                      (local-set-key (kbd "<down>") 'comint-next-input))))


(add-hook 'inferior-python-mode-hook
  	  (function (lambda ()
                      (acg/local-set-minor-mode-key 'company-mode-map (kbd "<tab>") 'python-shell-completion-complete-or-indent)
                      (local-set-key (kbd "<tab>") 'python-shell-completion-complete-or-indent))))
