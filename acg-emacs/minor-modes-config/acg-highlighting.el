;;; Packages for highlighting the buffer in smart ways

;; Highlight region when pasting, undoing/redoing, killing/deleting
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  ;; Face colors
  (set-face-background 'goggles-changed "yellow")
  (set-face-background 'goggles-removed "red")
  (set-face-background 'goggles-added "green")
  ;; Timing configs
  (setq goggles-pulse-iterations 10)
  (setq goggles-pulse-delay 0.03)
  (setq goggles-pulse t) ;; keep highlighted or just pulse highlight
  )

;; Highlighting selected words; VSCode-like.
(use-package symbol-overlay
  :config
  (setq symbol-overlay-map (make-sparse-keymap))  ;; Remove all default keybindings.

  :bind (("M-h M-h" . symbol-overlay-put)
         ("M-h u" . symbol-overlay-remove-all)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)))
