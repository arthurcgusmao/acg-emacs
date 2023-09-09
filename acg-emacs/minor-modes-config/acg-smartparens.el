;; Smartparens
(use-package smartparens
  :config
  (require 'smartparens-config)

  ;; Add new pairs
  (sp-pair "<" ">")

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


  ;; Utility functions

  (defun acg/sp-replace-pair (&optional pair)
    "Replace the wrapping pair of current sexp."
    (interactive)
    (let* ((fw-sexp (sp-get-sexp))
           (bw-sexp (sp-get-sexp t))
           (at-end nil)
           ;; Select target sexp
           (sexp (cond
                  ((eq (sp-get bw-sexp :end) (point))
                   (progn (setq at-end t) bw-sexp))
                  ((eq (sp-get fw-sexp :beg) (point))
                   fw-sexp)
                  (t
                   (sp-get-enclosing-sexp)))))
      (save-mark-and-excursion
        ;; Replace sexp pair
        (goto-char (sp-get sexp :beg))
        (sp-mark-sexp)
        (sp-wrap-with-pair
         (or pair
             (completing-read "Replace pair: "
                              (mapcar 'car sp-pair-list))))
        (sp-unwrap-sexp))
      ;; Return cursor to end of sexp for consistency
      (when at-end
        (sp-forward-sexp))))

  (defun acg/sp-cycle-pair ()
    "Cycle the wrapping pair of current sexp."
    (interactive)
    ;; @todo
    )


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
     ("M-j")
     ("M-F")
     ("M-B")

     ;; Set new bindings -- Reference: https://ebzzry.github.io/emacs-pairs.html
     ("C-0" . sp-forward-slurp-sexp)
     ("C-9" . sp-backward-slurp-sexp)
     ("C-)" . sp-forward-barf-sexp)
     ("C-(" . sp-backward-barf-sexp)
     ("C-j" . sp-join-sexp)
     ("M-0" . sp-forward-barf-sexp)
     ("M-9" . sp-backward-barf-sexp)
     ("M-)" . sp-unwrap-sexp)
     ("C-<right>" . sp-forward-sexp)
     ("C-<left>" . sp-backward-sexp)
     ("s-t" . sp-transpose-sexp)
     ("s-T" . sp-transpose-hybrid-sexp)
     ("<M-delete>")
     ("M-[" . acg/sp-cycle-pair)
     ("M-{" . acg/sp-replace-pair)
     )))

;; Related keybinding
;; @TODO: rebind function below
;; (global-set-key (kbd "M-w t") 'transpose-words)
