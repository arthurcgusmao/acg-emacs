;; (use-package project
;;   :straight nil
;;   :bind (("M-o" . project-find-file)))

(use-package projectiny
  :after project
  :straight (:host github :repo "arthurcgusmao/projectiny")
  :bind
  (("M-o" . projectiny-find-file)
   ("M-O" . projectiny-find-file-all)
   ("C-M-o" . projectiny-find-file-in)
   ("C-c p" . projectiny-find-file-in)))
