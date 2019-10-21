;; ibuffer is a better built-in buffer manager
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; Sort buffers per projectile project
(use-package ibuffer-projectile
  :ensure t
  :after (ibuffer
          projectile)
  :config
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-sidebar
  :ensure t
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "Sans Serif")))
