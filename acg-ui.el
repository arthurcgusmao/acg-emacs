;; Themes

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path (concat acg/acg-emacs-dir "themes"))
(load-theme 'acg-dark t)


;; Remove unnecessary things

;; Hide toolbar, menubar and scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable startup screen
(setq inhibit-startup-screen t) 


;; Help/Info Menus

;; show available keybindings after you start typing
(which-key-mode +1)


;; Point (Cursor)

;; make cursor a bar instead of rectangle
(setq-default cursor-type '(bar . 2))
(setq cursor-type '(bar . 2))
(setq cursor-in-non-selected-windows 'hollow)

;; disable blinking cursor
(blink-cursor-mode -1)

;; highlight current line
(global-hl-line-mode 1)

;; prevent highlighted line from appearing in all windows
(setq hl-line-sticky-flag nil)


;; Scrolling

;; restore point (cursor) position when scrolling or marking w/ ESC or C-g
(use-package restore-point
  :straight (:host github :repo "arthurcgusmao/restore-point")
  :ensure t
  :config
  (dolist (f '(acg/mark-dwim
               er/mark-defun))
    (add-to-list 'rp/restore-point-commands f t))
  :hook (after-init . restore-point-mode))

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
In case it needs to force splitting when current window is the
only one on its frame and its size is smaller than the split
thresholds, then it goes for vertical (below) splitting."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
	     ;; Split window horizontally.
	     (with-selected-window window
	       (split-window-right)))
        (and (window-splittable-p window)
	     ;; Split window vertically.
	     (with-selected-window window
	       (split-window-below)))
        (and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it disregarding the value of threshold.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
	 (not (window-minibuffer-p window))
	 (let ((split-height-threshold 0))
	   (when (window-splittable-p window)
	     (with-selected-window window
	       (split-window-below))))))))

(setq split-window-preferred-function #'acg/split-window-sensibly)

(defun acg/display-buffer-respect-thresholds (buffer alist)
  "Similar to display-buffer but displays on the same
window (instead of forcefully splitting) when splitting
thresholds are not met."
  (let ((window (selected-window)))
    (unless (or (window-splittable-p window t)
                (window-splittable-p window)
                (not (one-window-p)))
      (display-buffer-same-window buffer alist))))

;; Set custom splitting behavior as default
(setq display-buffer-base-action '(acg/display-buffer-respect-thresholds))
;; Filter out buffers that should split regardless of thresholds
(setq display-buffer-alist nil)
(append-to-list
 'display-buffer-alist
 `(("^\*jupyter-repl.*" . ,display-buffer-fallback-action)
   ))

;; Select help buffer after displaying it
(setq help-window-select t)


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



;; Configure font in MS-Windows
(defun acg/fontify-frame-mswindows (frame)
  (set-frame-parameter frame 'font "Consolas-10"))

(if (string-equal system-type "windows-nt")
    (progn (acg/fontify-frame-mswindows nil) ;; Fontify current frame
           (push 'acg/fontify-frame-mswindows after-make-frame-functions))) ;; Fontify any future frames


;; Configure font in Remote Environments, usually using XLaunch. For whatever
;; reason Emacs does not start with XFCE4's default monospace font
(defun acg/set-custom-frame-font (&optional frame)
  "Set my custom font for the frame.

Use (getenv \"DISPLAY\") to conditionally apply font
configuration. Usually the DISPLAY variable only contains
\"localhost\" when Forwarding X11 through an SSH connection,
which is when we want to apply these configurations
(because in those cases Emacs starts with a strange font)."
  (interactive)
  (with-selected-frame (or frame (selected-frame))
    (when (string-match-p (regexp-quote "localhost") (getenv "DISPLAY")) ;; Condition based on value of DISPLAY
      (cond
       ((>= (display-pixel-height) 2160) (set-frame-parameter frame 'font "Hack-15")) ; 4k resolution
       ((>= (display-pixel-height) 1440) (set-frame-parameter frame 'font "Hack-10.5")) ; 2560x1440 resolution
       (t (set-frame-parameter frame 'font "Hack-9"))))))

(acg/set-custom-frame-font (selected-frame)) ; Conditionally fontify current frame, if any
(add-to-list 'after-make-frame-functions #'acg/set-custom-frame-font) ; Conditionally fontify any future frames


(provide 'acg-ui)
