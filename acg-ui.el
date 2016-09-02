;; -------------------------------------------------------------------------
;; UI, Themes & Cursor
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)
(setq-default cursor-type 'bar) ;; makes cursor a bar instead of rectangle
(blink-cursor-mode -1) ;; disable blinking cursor
(setq inhibit-startup-screen t) ;; disable startup screen

;; diminish keeps the modeline tidy
(require 'diminish)

;; never lose the cursor again
(beacon-mode +1) 
(diminish 'beacon-mode)

;; show available keybindings after you start typing
(which-key-mode +1)
(diminish 'which-key-mode)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)



(provide 'acg-ui)
