;; ibuffer is a better built-in buffer manager
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-name-map
         ("<mouse-1>" . ibuffer-visit-buffer)
         ("<C-mouse-1>" . ibuffer-mouse-toggle-mark)))

;; Group buffers per Projectile project
(use-package ibuffer-projectile
  :ensure t
  :after (ibuffer
          projectile)
  :config
  (defun acg/ibuffer-projectile-run ()
    "Set up `ibuffer-projectile'."
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))

  (add-hook 'ibuffer-sidebar-mode-hook #'acg/ibuffer-projectile-run)
  (add-hook 'ibuffer-hook #'acg/ibuffer-projectile-run)
  :custom
  (ibuffer-projectile-prefix "Proj: "))

;; Sidebar
(use-package ibuffer-sidebar
  :ensure t
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "Sans Serif" :height 110)))
