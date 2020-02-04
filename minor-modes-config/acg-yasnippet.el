(require 'yasnippet)
(require 'ivy-yasnippet)

(add-to-list 'yas-snippet-dirs
             (concat acg/acg-emacs-dir "snippets"))

(yas-global-mode t)

;; keybindings
(define-key yas-minor-mode-map (kbd "C-c <tab>") 'ivy-yasnippet)
