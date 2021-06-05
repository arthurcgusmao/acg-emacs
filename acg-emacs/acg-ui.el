;; Themes

(use-package zenburn-theme)
(use-package anti-zenburn-theme)
(use-package modus-themes
  :straight (:host gitlab :repo "protesilaos/modus-themes"))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path (concat acg/acg-emacs-dir "themes"))
(setq custom-safe-themes t)             ; Never ask to confirm when loading themes
(load-theme 'acg-dark t)
(global-set-key (kbd "M-<f12>") 'acg/acg-themes-toggle)


;; Remove unnecessary things

;; Hide toolbar, menubar and scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable startup screen
(setq inhibit-startup-screen t)


;; Help/Info Menus

;; show available keybindings after you start typing
(use-package which-key
  :hook (after-init . which-key-mode))


;; Point (Cursor)

;; make cursor a bar instead of rectangle
(setq-default cursor-type '(bar . 2))
(setq-default cursor-in-non-selected-windows 'hollow)

;; disable blinking cursor
(blink-cursor-mode -1)

;; highlight current line
(global-hl-line-mode 1)

;; prevent highlighted line from appearing in all windows
(setq hl-line-sticky-flag nil)


;; Scrolling

;; restore point (cursor) position when scrolling or marking w/ ESC or C-g
(use-package restore-point
  :straight (:host github :protocol ssh
                   :repo "arthurcgusmao/restore-point")
  :config
  (dolist (f '(;; For marking regions
               acg/mark-dwim er/mark-defun
               ;; For movements across words, sexps, or related
               acg/right-subword acg/left-subword
               left-word right-word
               sp-backward-sexp sp-forward-sexp
               down-list backward-up-list python-nav-backward-up-list
               backward-paragraph forward-paragraph
               ;; For movements in the same line
               back-to-indentation beginning-of-line beginning-of-visual-line
               acg/beginning-of-visual-line-or-indentation move-end-of-line
               ))
    (add-to-list 'rp/restore-point-commands f t))
  :bind
  ("<S-escape>" . rp/point-ring-nav-previous)
  :hook (after-init . restore-point-mode))

;; nice scrolling
(setq scroll-margin 10
      scroll-conservatively 15
      scroll-preserve-screen-position t)

;; mouse scrolling
(use-package mwheel
  :straight nil
  :config
  (setq mouse-wheel-scroll-amount
        '(0.02 ((shift) . 0.06) ((control) . nil))) ; scroll wheel move 3 lines per scroll
  (setq mouse-wheel-progressive-speed nil) ; scroll speed always steady
  (setq mouse-wheel-tilt-scroll t)         ; scroll right/left
  (setq mouse-wheel-flip-direction t))     ; flip right/left direction

(use-package mouse-drag
  :config
  ;; disable middle-mouse button pasting
  (global-set-key [mouse-2] nil)
  ;; let middle-mouse button drag scroll (mouse throwing)
  (setq mouse-throw-with-scroll-bar t)
  (setq mouse-drag-electric-col-scrolling t)
  :bind
  (([down-mouse-2] . mouse-drag-throw)))
;; ([down-mouse-2] . mouse-drag-drag)


;; Emacs Frame and Windows - Title / Size / Splitting / Highlighting

(setq frame-title-format "Emacs")

(setq default-frame-alist
      '((bottom + 0) (right + 0)        ; initial position
        (width . 214) (height . 69)     ; initial size
        (vertical-scroll-bars . nil)))  ; disable vertical bars

;; Allow splitting current window if size above one of these thresholds
(setq split-width-threshold 160) ; How many columns to split side-by-side
(setq split-height-threshold 80) ; How many lines to split above/below

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
(use-package auto-dim-other-buffers
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (when (fboundp 'auto-dim-other-buffers-mode)
                (auto-dim-other-buffers-mode 0)
                (set-face-background 'auto-dim-other-buffers-face (face-background 'fringe))
                ;; (set-face-background 'auto-dim-other-buffers-face "#445")
                ))))

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



;; Font configurations

;; Configure font in MS-Windows
(defun acg/fontify-frame-mswindows (frame)
  (set-frame-parameter frame 'font "Consolas-10"))

(if (string-equal system-type "windows-nt")
    (progn (acg/fontify-frame-mswindows nil) ;; Fontify current frame
           (push 'acg/fontify-frame-mswindows after-make-frame-functions))) ;; Fontify any future frames


;; Configure font in Remote Environments, usually using XLaunch. For whatever
;; reason Emacs does not start with XFCE4's default monospace font
(defun acg/set-custom-frame-font (&optional frame)
  "Set my custom font configs for the frame."
  (interactive)
  (with-selected-frame (or frame (selected-frame))
    ;; Use (getenv "DISPLAY") to conditionally apply font
    ;; configuration. Usually the DISPLAY variable only contains
    ;; "localhost" when Forwarding X11 through an SSH connection,
    ;; which is when we want to apply these configurations
    ;; (because in those cases Emacs starts with a strange font).
    (when (string-match-p (regexp-quote "localhost") (getenv "DISPLAY")) ;; Condition based on value of DISPLAY
      (cond
       ((>= (display-pixel-height) 2160) (set-frame-parameter frame 'font "Hack-15")) ; 4k resolution
       ((>= (display-pixel-height) 1440) (set-frame-parameter frame 'font "Hack-10")) ; 2560x1440 resolution
       (t (set-frame-parameter frame 'font "Hack-9"))))

    ;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
    (set-fontset-font t 'symbol "Noto Color Emoji")
    (set-fontset-font t 'symbol "Apple Color Emoji" nil 'append)
    (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
    (set-fontset-font t 'symbol "Symbola" nil 'append)))

(acg/set-custom-frame-font (selected-frame)) ; Conditionally fontify current frame, if any
(add-to-list 'after-make-frame-functions #'acg/set-custom-frame-font) ; Conditionally fontify any future frames


;; Resize the whole frame, and not only a window
;; Adapted from https://stackoverflow.com/a/24714383/5103881
(defun acg/zoom-frame (&optional amt frame)
  "Increaze FRAME font size by amount AMT. Defaults to selected
frame if FRAME is nil, and to 1 if AMT is nil."
  (interactive "p")
  (let* ((frame (or frame (selected-frame)))
         (font (face-attribute 'default :font frame))
         (size (font-get font :size))
         (amt (or amt 1))
         (new-size (+ size amt)))
    (set-frame-font (font-spec :size new-size) t `(,frame))
    (message "Frame's font new size: %d" new-size)))

(defun acg/zoom-frame-out (&optional amt frame)
  "Call `acg/zoom-frame' with negative argument."
  (interactive "p")
  (acg/zoom-frame (- (or amt 1)) frame))

(global-set-key (kbd "C-x C-=") 'acg/zoom-frame)
(global-set-key (kbd "C-x C--") 'acg/zoom-frame-out)
(global-set-key (kbd "<C-down-mouse-4>") 'acg/zoom-frame)
(global-set-key (kbd "<C-down-mouse-5>") 'acg/zoom-frame-out)



;;; Mode-line configurations

;; http://emacs-fu.blogspot.com.br/2011/08/customizing-mode-line.html
;; http://amitp.blogspot.com.br/2011/08/emacs-custom-mode-line.html
;; http://www.lunaryorn.com/posts/make-your-emacs-mode-line-more-useful.html

(use-package emacs
  :straight nil
  :config
  (make-face 'acg/mode-line-common)
  (set-face-attribute 'acg/mode-line-common nil
                      ;; :foreground "#777"
                      :foreground (face-attribute 'mode-line :foreground)
                      :weight 'ultralight
                      )
  (make-face 'acg/mode-line-common-bold)
  (set-face-attribute 'acg/mode-line-common-bold nil
                      :foreground (face-attribute 'acg/mode-line-common :foreground)
                      :weight 'bold
                      )

  ;; use setq-default to set it for /all/ modes
  (setq-default mode-line-format
                (list
                 "%n " ; Print "Narrow" if region is narrowed
                 ;; was this buffer modified since the last save?
                 '(:eval (if buffer-read-only
                             " "
                           (if (buffer-modified-p) " ✏️" " 💾")))

                 ;; the buffer name; the file name as a tool tip
                 '(:eval (propertize " %b" 'face 'acg/mode-line-common-bold))

                 ;; Line & Column numbers -- Use '%02l' to set to 2 chars at least; may prevent flickering
                 "   (%l,%c)"
                 ;; Relative position, size of file
                 " %p" ;; % above top
                 ;; Current major mode for the buffer.
                 "   📃 %m"

                 ;; ;; Project indication -- Do not show for remote files
                 ;; "  "
                 ;; ;; Projectile indicator
                 ;; ;; '(:eval (format " 📁 %s" (projectile-project-name)))
                 ;; ;; version control indicator
                 ;; '(:eval (when (stringp vc-mode)
	         ;;           '(" 🔀" (vc-mode vc-mode))))

                 ;; list of minor modes
                 ;; minor-mode-alist
                 )))
