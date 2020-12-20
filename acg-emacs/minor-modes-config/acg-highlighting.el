;;; Packages for highlighting the buffer in smart ways


(use-package highlight)
(global-unset-key (kbd "M-h"))
;; changes the keymap to M-h
(global-set-key (kbd "M-h") hlt-map)
;; add custom keymaps that I find easier to remember
(global-set-key (kbd "M-h u a") 'hlt-unhighlight-all-prop)

;; @todo: Switch workflow to use hi-lock


;; Highlight region when pasting, undoing/redoing, killing/deleting
(use-package goggles
  :straight (:host github :repo "minad/goggles")
  :config
  (goggles-mode)
  (setq goggles-pulse t)) ;; keep highlighted or just pulse highlight
