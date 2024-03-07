;; ibuffer is a better built-in buffer manager
(use-package ibuffer
  :config

  (define-ibuffer-filter unsaved-file-buffers
      "Toggle current view to buffers whose file is unsaved."
    (:description "file is unsaved")
    (ignore qualifier)
    (and (buffer-local-value 'buffer-file-name buf)
         (buffer-modified-p buf)))

  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("/ u" . ibuffer-filter-by-unsaved-file-buffers)
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
