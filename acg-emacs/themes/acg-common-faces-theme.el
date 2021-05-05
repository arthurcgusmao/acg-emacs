(deftheme acg-common-faces
  "A theme for loading common faces to other themes.")

(custom-theme-set-faces
 'acg-common-faces

 ;; minimap
 '(minimap-active-region-background
   ((((background dark)) (:background "#2A2A2A222222"))
    (t (:background "#D3D3D3222222"))) nil :group)

 ;; markdown and org-mode
 '(markdown-header-face-1 ((t (:inherit outline-1 :foreground "#0CF" :weight bold :height 1.5))))
 '(markdown-header-face-2 ((t (:inherit outline-2 :foreground "#0EA" :weight bold :height 1.3))))
 '(markdown-header-face-3 ((t (:inherit outline-3 :foreground "#9E0" :weight bold :height 1.15))))
 '(markdown-header-face-4 ((t (:inherit outline-4 :foreground "#CC0" :weight bold :height 1.0))))
 '(markdown-header-face-5 ((t (:inherit outline-5 :foreground "#EA0" :weight bold :height 1.0))))
 '(markdown-header-face-6 ((t (:inherit outline-6 :foreground "#D60" :weight bold :height 1.0))))
 '(markdown-header-face-7 ((t (:inherit outline-7 :foreground "#90E" :weight bold :height 1.0))))
 '(markdown-header-face-8 ((t (:inherit outline-8 :foreground "#0FF" :weight bold :height 1.0))))
 '(org-level-1 ((t (:inherit outline-1 :foreground "#0CF" :weight bold :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "#0EA" :weight bold :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "#9E0" :weight bold :height 1.15))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "#CC0" :weight bold :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "#EA0" :weight bold :height 1.0))))
 '(org-level-6 ((t (:inherit outline-6 :foreground "#D60" :weight bold :height 1.0))))
 '(org-level-7 ((t (:inherit outline-7 :foreground "#90E" :weight bold :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :foreground "#0FF" :weight bold :height 1.0))))

 ;; parent matching
 ;; '(show-paren-match
 ;;   ((t (:background "#085" :foreground "white"))))
 ;; '(show-paren-mismatch
 ;; ((t (:background "#E04151" :foreground "white"))))
 '(show-paren-match
   ((t (:foreground "green" :weight bold))))
 '(show-paren-mismatch
   ((t (:foreground "red" :weight bold))))

 ;; tab-line
 '(tab-line ((t (:inherit default :inherit variable-pitch :background "#3d3c3a"
                          :foreground "#999" :overline nil :underline nil
                          :height 0.9))))
 '(tab-line-tab ((t (:inherit tab-line :background "#8d8986" :foreground "#333"))))
 '(tab-line-tab-inactive ((t (:inherit tab-line-tab :background "#7d7a77"))))
 '(tab-line-tab-current
   ((t (:inherit tab-line-tab :background "#BBBBBF"))))
 '(tab-line-highlight ((t (:inherit tab-line-tab :background "#FA9"))))

 ;; linum
 '(linum ((t (:inherit fringe :weight normal :foreground "#6d6c6a"))))
 )


;;; Functions for handling themes

(defun acg/disable-all-themes ()
  "Loop over all enabled themes and disable them."
  (interactive)
  (mapcar 'disable-theme custom-enabled-themes))

(defun acg/load-theme-disable-others (&optional theme)
  "Loads a single theme, deactivating all of the others."
  (interactive)
  (acg/disable-all-themes)
  (if theme
      (load-theme theme t)
    (call-interactively 'load-theme)))

(defun acg/acg-theme-switch ()
  "Switches between the light and dark themes with a single
command."
  (interactive)
  (let ((next-theme 'acg-dark))
    (when (member 'acg-dark custom-enabled-themes)
      (setq next-theme 'acg-light))
    (acg/load-theme-disable-others next-theme)))


(provide-theme 'acg-common-faces)
