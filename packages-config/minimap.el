(require 'minimap)

;; moving the minimap window to the right of the screen
(setq minimap-window-location 'right)

;; making minimap work with web mode
(add-to-list 'minimap-major-modes 'html-mode)
(add-to-list 'minimap-major-modes 'css-mode)
(add-to-list 'minimap-major-modes 'scss-mode)
(add-to-list 'minimap-major-modes 'web-mode)

;; starts minimap
(minimap-create)
