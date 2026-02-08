;;; Generative AI configurations.

(use-package agent-shell
  :straight (:host github :protocol ssh
                   :repo "xenodium/agent-shell")

  :bind
  (("C-c a a" . agent-shell)
   ("C-c a n" . agent-shell-new-shell)

   :map agent-shell-mode-map
   ;; Enable enter as a newline and C-c C-c to submit.
   ("RET" . newline)
   ("M-<return>" . shell-maker-submit)
   ;; Other configs.
   ("C-e" . agent-shell-other-buffer)
   ))
