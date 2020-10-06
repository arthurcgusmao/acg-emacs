;; Smartparens
(use-package smartparens
  :config
  (require 'smartparens-config)

  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (smartparens-global-mode t)

  ;; Enable smartparens' highlighting mode and Disable emacs' default
  (show-smartparens-global-mode t)
  (show-paren-mode -1)

  ;; Do not unselect region after wrapping it w/ pair
  (advice-add 'sp-wrap :after #'acg/with-mark-active)

  ;; Remove default keybindings and set new ones
  (sp--update-override-key-bindings
   'sp-override-key-bindings
   '(;; Unset paredit bindings
     ("M-s")
     ("M-<up>")
     ("M-<down>")
     ("M-r")
     ("C-}")
     ("C-{")
     ("M-S")

     ;; Unset smartparens bindings
     ("C-M-f")
     ("C-M-b")
     ("C-M-d")
     ("C-M-a")
     ("C-S-d")
     ("C-S-a")
     ("C-M-e")
     ("C-M-u")
     ("C-M-n")
     ("C-M-p")
     ("C-M-k")
     ("C-M-w")
     ("M-<delete>")
     ("M-<backspace>")
     ("C-<right>")
     ("C-<left>")
     ("C-M-<left>")
     ("C-M-<right>")
     ("M-D")
     ("C-M-<delete>")
     ("C-M-<backspace>")
     ("C-S-<backspace>")
     ("C-]")
     ("C-M-]")
     ("M-F")
     ("M-B")

     ;; Set new bindings -- Reference: https://ebzzry.github.io/emacs-pairs.html
     ("C-0" . sp-forward-slurp-sexp)
     ("C-9" . sp-backward-slurp-sexp)
     ("C-)" . sp-forward-barf-sexp)
     ("C-(" . sp-backward-barf-sexp)
     ("M-0" . sp-forward-barf-sexp)
     ("M-9" . sp-backward-barf-sexp)
     ("M-)" . sp-unwrap-sexp)
     ("M-<right>" . sp-forward-sexp)
     ("M-<left>" . sp-backward-sexp)
     ("M-t" . sp-transpose-sexp)
     ("M-S-T" . sp-transpose-hybrid-sexp)
     ("<M-delete>"))))

;; Related keybinding
;; @TODO: rebind function below
;; (global-set-key (kbd "M-w t") 'transpose-words)
