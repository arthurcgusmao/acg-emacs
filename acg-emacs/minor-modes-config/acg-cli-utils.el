;; Parse CLI commands into an Emacs buffer with clickable sub-commands.
;; Useful for commands such as aws, kubectl, etc., that don't have man pages.
(use-package noman
  :straight (noman :type git
                   :host github
                   :repo "andykuszyk/noman.el"))
