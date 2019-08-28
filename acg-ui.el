;; themes

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path (concat acg-emacs-dir "themes"))
(load-theme 'acg-dark t)


;; removing unnecessary things

;; hide toolbar, menubar and scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t) 

;; diminish keeps the modeline tidy
(require 'diminish)


;; help/info menus

;; show available keybindings after you start typing
(which-key-mode +1)
(diminish 'which-key-mode)


;; cursor

;; makes cursor a bar instead of rectangle
(setq-default cursor-type '(bar . 2))
(setq cursor-type '(bar . 2))
(setq cursor-in-non-selected-windows 'hollow)

;; disable blinking cursor
(blink-cursor-mode -1)

;; highlight current line
(global-hl-line-mode 1)

;; prevent highlighted line from appearing in all windows
(setq hl-line-sticky-flag nil)


;; scrolling

;; ;; restore the cursor to position when scrolling through the page
;; (require 'scroll-restore)
;; ;; Allow scroll-restore to modify the cursor face
;; (setq scroll-restore-handle-cursor t)
;; ;; Make the cursor invisible while POINT is off-screen
;; (setq scroll-restore-cursor-type nil)
;; ;; Jump back to the original cursor position after scrolling
;; (setq scroll-restore-jump-back t)
;; ;; Restores the highlighted line by hl-line mode
;; ;; (setq scroll-restore-handle-global-hl-line-mode t)
;; (scroll-restore-mode 0)

;; always keep some lines at bottom/top when scroll with keypad
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; scroll wheel move 3 lines per scroll
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
;; scroll speed always steady
(setq mouse-wheel-progressive-speed nil)

;; nice scrolling
(setq scroll-margin 0 ; never recenters window
      scroll-conservatively 100000
      scroll-preserve-screen-position t)


;; windows - title / size / splitting / highlighting

(setq frame-title-format "Emacs")

(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 100))

;; makes new buffers always default to vertical splitting (instead of horizontal)
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; always opens help buffer in the same window
(add-to-list 'display-buffer-alist
             '("Help" display-buffer-same-window))
(add-to-list 'display-buffer-alist
             '("Magit" display-buffer-same-window))
(add-to-list 'display-buffer-alist
             '("Anaconda" display-buffer-same-window))

;; unhighlight inactive windows
(diminish 'auto-dim-other-buffers-mode)
(add-hook 'after-init-hook
          (lambda ()
            (when (fboundp 'auto-dim-other-buffers-mode)
              (auto-dim-other-buffers-mode 0)
              (set-face-background 'auto-dim-other-buffers-face (face-background 'fringe))
              ;; (set-face-background 'auto-dim-other-buffers-face "#445")
              )))


;; configure font in Windows
(if (string-equal system-type "windows-nt")
    (and (set-face-attribute 'default nil :family "Consolas" :height 110)))


(provide 'acg-ui)
