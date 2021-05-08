(deftheme acg-common-faces
  "A theme for loading common faces to other themes.")

(custom-theme-set-faces
 'acg-common-faces

 ;; minimap
 '(minimap-active-region-background
   ((((background dark)) (:background "#2A2A2A222222"))
    (t (:background "#D3D3D3222222"))) nil :group)

 ;; Outline (inherited by markdown and org)
 '(outline-1 ((t (:foreground "#0CF" :weight bold :height 1.50))))
 '(outline-2 ((t (:foreground "#0EA" :weight bold :height 1.30))))
 '(outline-3 ((t (:foreground "#9E0" :weight bold :height 1.15))))
 '(outline-4 ((t (:foreground "#CC0" :weight bold :height 1.00))))
 '(outline-5 ((t (:foreground "#EA0" :weight bold :height 0.95))))
 '(outline-6 ((t (:foreground "#D60" :weight bold :height 0.90))))
 '(outline-7 ((t (:foreground "#90E" :weight bold :height 0.85))))
 '(outline-8 ((t (:foreground "#0FF" :weight bold :height 0.80))))
 ;; ;; markdown and org-mode
 '(markdown-header-face-1 ((t (:inherit outline-1))))
 '(markdown-header-face-2 ((t (:inherit outline-2))))
 '(markdown-header-face-3 ((t (:inherit outline-3))))
 '(markdown-header-face-4 ((t (:inherit outline-4))))
 '(markdown-header-face-5 ((t (:inherit outline-5))))
 '(markdown-header-face-6 ((t (:inherit outline-6))))
 '(markdown-header-face-7 ((t (:inherit outline-7))))
 '(markdown-header-face-8 ((t (:inherit outline-8))))
 '(markdown-markup-face ((t (:foreground "#888"))))

 '(org-document-title ((t (:inherit org-document-info :weight bold :height 1.7))))
 '(org-document-info ((t (:foreground "#90E"))))
 '(org-document-info-keyword ((t (:inherit org-meta-line))))

 ;; parent matching
 ;; '(show-paren-match
 ;;   ((t (:background "#085" :foreground "white"))))
 ;; '(show-paren-mismatch
 ;; ((t (:background "#E04151" :foreground "white"))))
 '(show-paren-match
   ((t (:foreground "green" :weight bold))))
 '(show-paren-mismatch
   ((t (:foreground "red" :weight bold))))

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
