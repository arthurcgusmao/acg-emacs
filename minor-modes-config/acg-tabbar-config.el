(require 'tabbar)
;; (require 'tabbar-ruler)

;; (setq tabbar-ruler-global-tabbar t)    ; get tabbar
;; (setq tabbar-ruler-global-ruler nil)   ; get global ruler
;; (setq tabbar-ruler-popup-menu nil)       ; get popup menu.
;; (setq tabbar-ruler-popup-toolbar nil)    ; get popup toolbar
;; (setq tabbar-ruler-popup-scrollbar nil)  ; show scroll-bar on mouse-move

(tabbar-mode 1)

(setq tabbar-cycle-scope 'tabs)
(setq tabbar-use-images nil)

(global-set-key (kbd "<C-tab>") 'tabbar-forward)
(global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-backward)


;; customize to show all normal files in one group
(defun my-tabbar-buffer-groups () 
  "Returns the name of the tab group names the current buffer belongs to.
 There are two groups: Emacs buffers (those whose name starts with '*', plus
 dired buffers), and the rest.  This works at least with Emacs v24.2 using
 tabbar.el v1.7."
  (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
              ((eq major-mode 'dired-mode) "emacs")
              (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)


;; ---------------------------------------------------------
;; Add a buffer modification state indicator in the label
;; ---------------------------------------------------------

;; Add a buffer modification state indicator in the tab label, and place a
;; space around the label to make it looks less crowd.
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat " * " (concat ad-return-value " "))
          (concat " " (concat ad-return-value " ")))))

;; Called each time the modification state of the buffer changed.
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

;; First-change-hook is called BEFORE the change is made.
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)

;; This doesn't work for revert, I don't know.
;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)


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
 '(tabbar-separator ((t (:inherit tabbar-default :background "#000"))))
 '(tabbar-unselected ((t (:inherit tabbar-default)))))
