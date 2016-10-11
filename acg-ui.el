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


;;;;;;;;;;;;;;;;;;;;;
;; scrolling
;;;;;;;;;;;;;;;;;;;;;

;; restore the cursor to position when scrolling through the page
(require 'scroll-restore)
(scroll-restore-mode 1)

;; always keep some lines at bottom/top when scroll with keypad
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; scroll wheel move one line per scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
;; scroll speed always steady
(setq mouse-wheel-progressive-speed nil)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;;;;;;;;;;;;;;;;;;;;;
;; window size
;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist '(height . 120))
(add-to-list 'default-frame-alist '(width . 160))



(provide 'acg-ui)
