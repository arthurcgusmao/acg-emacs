(require 'minimap)
(diminish 'minimap-mode)

;; moving the minimap window to the right of the screen
(setq minimap-window-location 'right)

;; making minimap work with web mode
(add-to-list 'minimap-major-modes 'html-mode)
(add-to-list 'minimap-major-modes 'css-mode)
(add-to-list 'minimap-major-modes 'scss-mode)
(add-to-list 'minimap-major-modes 'web-mode)

;; changing colors
(custom-set-faces
  '(minimap-active-region-background
    ((((background dark)) (:background "#2A2A2A222222"))
      (t (:background "#D3D3D3222222")))
    "Face for the active region in the minimap.
By default, this is only a different background color."
    :group 'minimap))

;; minimap size
(custom-set-variables
 '(minimap-width-fraction .18))

;; starts minimap
(minimap-create)
