;;;;;;;;;;;;;;;;;;;;;
;; themes
;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'leuven t)


;;;;;;;;;;;;;;;;;;;;;
;; removing unnecessary things
;;;;;;;;;;;;;;;;;;;;;

;; hide toolbar, menubar and scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t) ;; disable startup screen
;; (setq initial-buffer-choice 'custom-initial-buffer)

(defun custom-initial-buffer ()
  ;; (helm-find-files-1 "~/")
  (kill-buffer "*scratch*")
  "*Messages*")

;; removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; diminish keeps the modeline tidy
(require 'diminish)


;;;;;;;;;;;;;;;;;;;;;
;; help/info menus
;;;;;;;;;;;;;;;;;;;;;

;; show available keybindings after you start typing
(which-key-mode +1)
(diminish 'which-key-mode)

;; showing colors in HEX representations
(rainbow-mode t)

;;;;;;;;;;;;;;;;;;;;;
;; cursor
;;;;;;;;;;;;;;;;;;;;;

;; makes cursor a bar instead of rectangle
(setq-default cursor-type 'bar)
(setq cursor-type 'bar)
(setq cursor-in-non-selected-windows 'hollow)

;; disable blinking cursor
(blink-cursor-mode -1)

;; never lose the cursor again (beacon mode)
;; (beacon-mode +1) 
;; (diminish 'beacon-mode)

;; highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#FAF5FB")
(set-cursor-color "#A0B")
;; prevent highlighted line from appearing in all windows
(setq hl-line-sticky-flag nil)

;;;;;;;;;;;;;;;;;;;;;
;; scrolling
;;;;;;;;;;;;;;;;;;;;;

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

;; scroll wheel move one line per scroll
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
;; scroll speed always steady
(setq mouse-wheel-progressive-speed nil)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position t)


;;;;;;;;;;;;;;;;;;;;;
;; windows - title / size / splitting / highlighting
;;;;;;;;;;;;;;;;;;;;;

(setq frame-title-format "Emacs")

(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 120))

;; makes new buffers always default to vertical splitting (instead of horizontal)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; always opens help buffer in the same window
(add-to-list 'display-buffer-alist
            '("Help" display-buffer-same-window))

;; unhighlight inactive windows
(add-hook 'after-init-hook
          (lambda ()
            (when (fboundp 'auto-dim-other-buffers-mode)
              (auto-dim-other-buffers-mode t)
              ;; (set-face-background 'auto-dim-other-buffers-face (face-background 'fringe)))))
              (set-face-background 'auto-dim-other-buffers-face "#CCCCCC"))))


(provide 'acg-ui)
