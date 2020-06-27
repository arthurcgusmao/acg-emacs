(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs
               (concat acg/acg-emacs-dir "snippets"))
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package ivy-yasnippet
  :after (yasnippet ivy)
  :bind
  (:map yas-minor-mode-map
        ("C-c <tab>" . ivy-yasnippet)))
