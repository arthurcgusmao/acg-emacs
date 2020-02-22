(require 'minimap)

;; moving the minimap window to the right of the screen
(custom-set-variables
 '(minimap-window-location 'right)
 ;; minimap size
 '(minimap-width-fraction .15)
 ;; trying out new things
 ;; '(minimap-dedicated-window nil)
 )



;; ;; making minimap work with certain modes
;; (add-to-list 'minimap-major-modes 'html-mode)
;; (add-to-list 'minimap-major-modes 'css-mode)
;; (add-to-list 'minimap-major-modes 'scss-mode)
;; (add-to-list 'minimap-major-modes 'web-mode)
;; (add-to-list 'minimap-major-modes 'text-mode)

;; ;; (add-to-list 'minimap-major-modes 'TeX)
;; ;; (add-to-list 'minimap-major-modes 'LaTeX)
;; ;; (add-to-list 'minimap-major-modes 'TeX-mode)
;; (add-to-list 'minimap-major-modes 'tex-mode)
;; ;; (add-to-list 'minimap-major-modes 'LaTeX-mode)
;; (add-to-list 'minimap-major-modes 'latex-mode)


;; changing colors
(custom-set-faces
 '(minimap-active-region-background
   ((((background dark)) (:background "#2A2A2A222222"))
    (t (:background "#D3D3D3222222")))
   "Face for the active region in the minimap.
By default, this is only a different background color."
   :group 'minimap))


;; removing the mode line in minimap buffer
;; (add-hook 'minimap-sb-mode-hook (lambda () (setq mode-line-format nil)))

;; starts minimap
;; (minimap-create)
;; (minimap-mode 0)
