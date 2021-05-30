(use-package projectiny
  :straight (:host github :protocol ssh
                   :repo "arthurcgusmao/projectiny")
  :config
  (setq projectiny-known-projects-file
        (concat acg/history-dir "projectiny-known-projects"))
  :bind
  (("M-o" . projectiny-find-file)
   ("M-O" . projectiny-find-file-all)
   ("C-M-o" . projectiny-find-file-in)
   ("C-c p" . projectiny-find-file-in))
  :commands
  (projectiny-add-project
   projectiny-clean-known-projects))
