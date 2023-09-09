;;; Packages for highlighting the buffer in smart ways

;; Highlight region when pasting, undoing/redoing, killing/deleting
(use-package goggles
  :straight (:host github :repo "minad/goggles")
  :config
  ;; Face colors
  (set-face-background 'goggles-changed "yellow")
  (set-face-background 'goggles-removed "red")
  (set-face-background 'goggles-added "green")
  ;; Timing configs
  (setq goggles-pulse-iterations 10)
  (setq goggles-pulse-delay 0.03)
  (setq goggles-pulse t) ;; keep highlighted or just pulse highlight
  (goggles-mode 1))
