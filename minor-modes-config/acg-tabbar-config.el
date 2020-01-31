(require 'tabbar)
;; (require 'tabbar-ruler)

;; (setq tabbar-ruler-global-tabbar t)    ; get tabbar
;; (setq tabbar-ruler-global-ruler nil)   ; get global ruler
;; (setq tabbar-ruler-popup-menu nil)       ; get popup menu.
;; (setq tabbar-ruler-popup-toolbar nil)    ; get popup toolbar
;; (setq tabbar-ruler-popup-scrollbar nil)  ; show scroll-bar on mouse-move


(setq tabbar-cycle-scope 'tabs)
(setq tabbar-use-images nil)

(global-set-key (kbd "<C-tab>") 'acg/tabbar-forward)
(global-set-key (kbd "<C-S-iso-lefttab>") 'acg/tabbar-backward) ;; for Linux
(global-set-key (kbd "<C-S-tab>") 'acg/tabbar-backward) ;; for Windows
(global-set-key (kbd "<C-next>") 'acg/tabbar-forward)
(global-set-key (kbd "<C-prior>") 'acg/tabbar-backward)
;; Key sequences "C-S-PgUp" and "C-S-PgDn" move the current tab to the left and to the right.
(global-set-key (kbd "C-S-<prior>") 'tabbar-move-current-tab-one-place-left)
(global-set-key (kbd "C-S-<next>") 'tabbar-move-current-tab-one-place-right)

;; removing tabbar display in some buffers
;; not necessary anymore because these modes are not even belonging to a group anymore
;; (add-hook 'help-mode-hook 'tabbar-local-mode)
;; (add-hook 'messages-buffer-mode-hook 'tabbar-local-mode)
;; (add-hook 'helm-major-mode-hook 'tabbar-local-mode)
;; (add-hook 'comint-mode-hook 'tabbar-local-mode)

;; ---------------------------------------------------------
;; Customizing which buffers are shown in tabbar
;; ---------------------------------------------------------

;; (defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
;;   "Returns the name of the tab group names the current buffer belongs to.
;;  There are two groups: Emacs buffers (those whose name starts with '*', plus
;;  dired buffers), and the rest.  This works at least with Emacs v24.2 using
;;  tabbar.el v1.7."
;;   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
;;               ((eq major-mode 'dired-mode) "emacs")
;;               (t "user"))))


(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
  "Returns the name of the tab group names the current buffer belongs to.
 There are two groups: Emacs buffers (those whose name starts with '*', plus
 dired buffers), and the rest.  This works at least with Emacs v24.2 using
 tabbar.el v1.7."
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (list (cond (t "user")))))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)


(defun switch-tab-group (group-name)
  "Switch to a specific tab group."
  (let ((tab-buffer-list (mapcar
                          #'(lambda (b)
                              (with-current-buffer b
                                (list (current-buffer)
                                      (buffer-name)
                                      (funcall tabbar-buffer-groups-function) )))
                          (funcall tabbar-buffer-list-function))))
    (catch 'done
      (mapc
       #'(lambda (group)
           (when (equal group-name (format "%s" (car (car (cdr (cdr group))))))
             (throw 'done (switch-to-buffer (car (cdr group))))))
       tab-buffer-list) )))


(defun acg/tabbar-forward ()
  (interactive)
  (if (string-equal (tabbar-buffer-tabs) "user")
      (tabbar-forward)
    (switch-tab-group "user")))

(defun acg/tabbar-backward ()
  (interactive)
  (if (string-equal (tabbar-buffer-tabs) "user")
      (tabbar-backward)
    (switch-tab-group "user")))

;; ---------------------------------------------------------
;; Add a buffer modification state indicator in the label
;; ---------------------------------------------------------

;; Add a buffer modification state indicator in the tab label, and place a
;; space around the label to make it looks less crowd.
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat " ✎ " (concat ad-return-value " "))
          (concat " " (concat ad-return-value " ")))))

;; Called each time the modification state of the buffer changed.
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))
(add-hook 'after-save-hook 'ztl-modification-state-change)

;; First-change-hook is called BEFORE the change is made.
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

;; Update when buffer unmodified -- requires `update-unmodified-buffer' to be loaded first
(add-to-list 'acg/unmodified-buffer-hook 'ztl-modification-state-change t)

;; ---------------------------------------------------------
;; face customization
;; ---------------------------------------------------------

(setq tabbar-background-color "#3d3c3a") ;; the color of the tabbar background
(custom-set-faces
 '(tabbar-default ((t (:inherit default :background "#3d3c3a" :foreground "#999" :weight medium
                                :overline nil :underline nil :height 0.9 :widthtype semi-condensed ))))
 '(tabbar-button ((t (:inherit tabbar-default :weight normal))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default))))
 '(tabbar-highlight ((t (:inherit tabbar-default :background "#FA9"))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "#BBBBBF" :foreground "#333"))))
 '(tabbar-selected-modified ((t (:inherit tabbar-selected))))
 '(tabbar-modified ((t (:inherit tabbar-default))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "#333"))))
 '(tabbar-unselected ((t (:inherit tabbar-default)))))


;; ---------------------------------------------------------
;; removing characters from the left (the ones that indicate scrolling)
;; ---------------------------------------------------------

(custom-set-variables
 '(tabbar-scroll-left-button
   (cons (cons "" tabbar-scroll-left-button-enabled-image)
         (cons "" nil)))
 '(tabbar-scroll-right-button
   (cons (cons "" tabbar-scroll-right-button-enabled-image)
         (cons "" nil)))
 '(tabbar-home-button
   (cons (cons " " tabbar-home-button-enabled-image)
         (cons " " tabbar-home-button-disabled-image)))
 '(tabbar-buffer-home-button
   (cons (cons " " tabbar-home-button-enabled-image)
         (cons " " tabbar-home-button-disabled-image)))
 )


;; Move tabs one place to the left/right
;; taken from https://www.emacswiki.org/emacs/TabBarMode

(defun tabbar-move-current-tab-one-place-left ()
  "Move current tab one place left, unless it's already the leftmost."
  (interactive)
  (let* ((bufset (tabbar-current-tabset t))
         (old-bufs (tabbar-tabs bufset))
         (first-buf (car old-bufs))
         (new-bufs (list)))
    (if (string= (buffer-name) (format "%s" (car first-buf)))
        old-bufs ; the current tab is the leftmost
      (setq not-yet-this-buf first-buf)
      (setq old-bufs (cdr old-bufs))
      (while (and
              old-bufs
              (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
        (push not-yet-this-buf new-bufs)
        (setq not-yet-this-buf (car old-bufs))
        (setq old-bufs (cdr old-bufs)))
      (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
          (progn
            (push (car old-bufs) new-bufs) ; this is the tab that was to be moved
            (push not-yet-this-buf new-bufs)
            (setq new-bufs (reverse new-bufs))
            (setq new-bufs (append new-bufs (cdr old-bufs))))
        (error "Error: current buffer's name was not found in Tabbar's buffer list."))
      (set bufset new-bufs)
      (tabbar-set-template bufset nil)
      (tabbar-display-update))))

(defun tabbar-move-current-tab-one-place-right ()
  "Move current tab one place right, unless it's already the rightmost."
  (interactive)
  (let* ((bufset (tabbar-current-tabset t))
         (old-bufs (tabbar-tabs bufset))
         (first-buf (car old-bufs))
         (new-bufs (list)))
    (while (and
            old-bufs
            (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
      (push (car old-bufs) new-bufs)
      (setq old-bufs (cdr old-bufs)))
    (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
        (progn
          (setq the-buffer (car old-bufs))
          (setq old-bufs (cdr old-bufs))
          (if old-bufs ; if this is false, then the current tab is the rightmost
              (push (car old-bufs) new-bufs))
          (push the-buffer new-bufs)) ; this is the tab that was to be moved
      (error "Error: current buffer's name was not found in Tabbar's buffer list."))
    (setq new-bufs (reverse new-bufs))
    (setq new-bufs (append new-bufs (cdr old-bufs)))
    (set bufset new-bufs)
    (tabbar-set-template bufset nil)
    (tabbar-display-update)))


(tabbar-mode 1)
