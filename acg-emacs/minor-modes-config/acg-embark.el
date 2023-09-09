(use-package embark
  :config
  ;; Configure embark to use which-key
  (use-package which-key
    :config
    (setq embark-action-indicator
      (lambda (map _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator))

  :commands (embark--quit-and-run) ; Used in consult configs

  :bind
  (("M-A" . embark-act)
   (:map minibuffer-local-map
         ("C-e" . embark-export))))


(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
