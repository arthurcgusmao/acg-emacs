(use-package highlight)
(global-unset-key (kbd "M-h"))
;; changes the keymap to M-h
(global-set-key (kbd "M-h") hlt-map)
;; add custom keymaps that I find easier to remember
(global-set-key (kbd "M-h u a") 'hlt-unhighlight-all-prop)

;; @todo: Switch workflow to use hi-lock
