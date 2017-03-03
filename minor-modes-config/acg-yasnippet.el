(yas-global-mode t)
(diminish 'yas-minor-mode)

;; the following dir must be cloned from
;;  https://github.com/AndreaCrotti/yasnippet-snippets
(add-to-list 'yas-snippet-dirs
             (concat acg-emacs-dir "packages-config/yasnippet-snippets"))
