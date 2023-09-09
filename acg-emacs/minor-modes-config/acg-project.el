(use-package projectiny
  :straight (:host github :protocol ssh
                   :repo "arthurcgusmao/projectiny")
  :config
  (setq projectiny-known-projects-file
        (concat acg/history-dir "projectiny-known-projects"))
  :bind
  (("M-p" . projectiny-find-file)
   ;; ("M-O" . projectiny-find-file-all)  ;; Enabling it messes up with the arrow keys in the terminal!
   ("C-M-o" . projectiny-find-file-in)
   ("C-c p" . projectiny-find-file-in))
  :commands
  (projectiny-add-project
   projectiny-clean-known-projects))


;; (defun acg/circumvent-project-files-remote (args)
;;   (when (file-remote-p default-directory)
;;     (let* ((proj-root (project-root (project-current nil)))
;;            (query (read-string
;;                    (format "Filter files in %s:" proj-root))))
;;       (shell-command "find ()")
;;       )))

;; (defun acg/cached-project-files (args)
;;   nil)

;; (advice-add 'acg/circumvent-project-files-remote
;;             :before #'project-files)
