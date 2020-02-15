;; themes

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path (concat acg/acg-emacs-dir "themes"))
(load-theme 'acg-dark t)


;; removing unnecessary things

;; hide toolbar, menubar and scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t) 


;; help/info menus

;; show available keybindings after you start typing
(which-key-mode +1)


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

;; nice scrolling
(setq scroll-margin 0 ; never recenters window
      scroll-conservatively 100000
      scroll-preserve-screen-position t)


(when (display-graphic-p)
  ;; scroll wheel move 3 lines per scroll
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
  ;; scroll speed always steady
  (setq mouse-wheel-progressive-speed nil))

;; disable middle-mouse button pasting
(global-set-key [mouse-2] nil)
;; let middle-mouse button drag scroll (mouse throwing)
(require 'mouse-drag)
(global-set-key [down-mouse-2] 'mouse-drag-throw)
;; (global-set-key [down-mouse-2] 'mouse-drag-drag)
(setq mouse-throw-with-scroll-bar t)
(setq mouse-drag-electric-col-scrolling t)


;; Windows - Title / Size / Splitting / Highlighting

(setq frame-title-format "Emacs")

(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(height . 80))

;; Allow splitting current window if size above one of these thresholds
(setq split-width-threshold 160) ; How many columns to split side-by-side
(setq split-height-threshold 100) ; How many lines to split above/below

;; Custom split window logic
(defun acg/split-window-sensibly (&optional window)
  "Similar to `split-window-sensibly', but prefers
horizontal (side by side) rather than vertical (below) splitting.
Also, it doesn't force splitting when current window is the only
one on its frame and its size is smaller than the split
thresholds as the original would."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
	     ;; Split window horizontally.
	     (with-selected-window window
	       (split-window-right)))
        (and (window-splittable-p window)
	     ;; Split window vertically.
	     (with-selected-window window
	       (split-window-below))))))

(setq split-window-preferred-function #'acg/split-window-sensibly)

;; Select help buffer after displaying it
(setq help-window-select t)

;; Always opens help buffer in the same window
;; (add-to-list 'display-buffer-alist
;;              '("Help" display-buffer-same-window))
;; (add-to-list 'display-buffer-alist
;;              '("Magit" display-buffer-same-window))
;; (add-to-list 'display-buffer-alist
;;              '("Anaconda" display-buffer-same-window))


;; Unhighlight inactive windows

;; Not working properly
(add-hook 'after-init-hook
          (lambda ()
            (when (fboundp 'auto-dim-other-buffers-mode)
              (auto-dim-other-buffers-mode 0)
              (set-face-background 'auto-dim-other-buffers-face (face-background 'fringe))
              ;; (set-face-background 'auto-dim-other-buffers-face "#445")
              )))

;; ;; Also didn't work properly
;; (defun highlight-selected-window ()
;;   "Highlight selected window with a different background color.
;; Adapted from https://emacs.stackexchange.com/a/36240/13589"
;;   (walk-windows (lambda (w)
;;                   (if (eq w (selected-window))
;;                       (with-current-buffer (window-buffer w)
;;                         (buffer-face-set '(:background "green"))
;;                         ;; (message "true `%s'" w)
;;                         )
;;                     (with-current-buffer (window-buffer w)
;;                       (buffer-face-set '(:background "red"))
;;                       ;; (message "true `%s'" w)
;;                       ))))
;;   (buffer-face-set 'default))
;; (add-hook 'buffer-list-update-hook 'highlight-selected-window)
;; ;; (remove-hook 'buffer-list-update-hook 'highlight-selected-window)



;; configure font in MS-Windows
(defun acg/fontify-frame-mswindows (frame)
  (set-frame-parameter frame 'font "Consolas-10"))

(if (string-equal system-type "windows-nt")
    (progn (acg/fontify-frame-mswindows nil) ;; Fontify current frame
           (push 'acg/fontify-frame-mswindows after-make-frame-functions))) ;; Fontify any future frames


(provide 'acg-ui)
