(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
  (setq-default web-mode-enable-auto-pairing t
                web-mode-enable-auto-opening t
                web-mode-enable-auto-indentation t
                web-mode-enable-block-face t
                web-mode-enable-part-face t
                ;; web-mode-enable-comment-keywords t
                web-mode-enable-css-colorization t
                web-mode-enable-current-element-highlight t
	        ;;web-mode-enable-current-column-highlight t
                web-mode-enable-heredoc-fontification t
                web-mode-enable-engine-detection t))

;; using web-mode with autocomplete
;;(require 'auto-complete-config)
;;(add-to-list 'ac-modes 'web-mode)
;;(setq web-mode-ac-sources-alist
;;  '(("css" . (ac-source-css-property))
;;    ("html" . (ac-source-words-in-buffer ac-source-abbrev))
;;    ;;("php" . (ac-source-words-in-buffer ac-source-abbrev
;;    ))
