;;; Generative AI configurations.

(use-package agent-shell
  :straight (:host github :protocol ssh
                   :repo "xenodium/agent-shell")

  :bind
  (("C-c a a" . agent-shell)
   ("C-c a n" . agent-shell-new-shell)
   :agent-shell-mode
   ("C-e" . agent-shell-other-buffer)))
