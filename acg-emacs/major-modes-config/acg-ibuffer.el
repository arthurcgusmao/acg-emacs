;; ibuffer is a better built-in buffer manager
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-name-map
         ("<mouse-1>" . ibuffer-visit-buffer)
         ("<M-mouse-1>" . ibuffer-mouse-toggle-mark)))

(use-package ibuffer-vc
  :after (ibuffer vc)
  :config
  (add-hook 'ibuffer-hook #'ibuffer-vc-set-filter-groups-by-vc-root))

(use-package ibuffer-sidebar
  :after (ibuffer ibuffer-vc)
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (add-hook 'ibuffer-sidebar-mode-hook #'ibuffer-vc-set-filter-groups-by-vc-root)
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "Sans Serif" :height 110)))
