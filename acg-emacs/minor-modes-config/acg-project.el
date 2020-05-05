(use-package project
  :straight nil
  :bind (("M-o" . project-find-file)))

(use-package projectiny
  :straight (:host github :repo "arthurcgusmao/projectiny")
  :bind
  (("C-c p" . projectiny-find-file-in)
   ("M-O" . projectiny-find-file-all)))
