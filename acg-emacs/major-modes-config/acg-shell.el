
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )

;; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

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


