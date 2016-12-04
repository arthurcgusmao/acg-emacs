;; smartparens
(require 'smartparens-config)
(diminish 'smartparens-mode)

(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)







(require 'smartparens-config)

;; smartparens for all modes
(smartparens-global-mode t)

;; enables smartparens' paren highlighting mode and disables emacs default
(show-smartparens-global-mode t) 
(show-paren-mode nil)


;; removing smartparens' default key-bindings (and setting new ones)
(custom-set-variables
 '(sp-override-key-bindings
   (quote (
           ;; unsetting paredit bindings
           ("M-s")
           ("M-<up>")
           ("M-<down>")
           ("M-r")
           ("C-)")
           ("C-}")
           ("C-(")
           ("C-{")
           ("M-S")
           
           ;; unsetting smartparens bindings
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

           ;; setting new bindings
           ;;    resources: https://ebzzry.github.io/emacs-pairs.html
           ("C-0" . sp-forward-slurp-sexp)
           ("C-9" . sp-backward-slurp-sexp)
           ("M-0" . sp-forward-barf-sexp)
           ("M-9" . sp-backward-barf-sexp)
           ("M-<right>" . sp-forward-sexp)
           ("M-<left>" . sp-backward-sexp)
           ("M-t" . sp-transpose-sexp)
           ("M-S-T" . sp-transpose-hybrid-sexp)
           ("<M-delete>")
           ))))

(sp--update-override-key-bindings)

