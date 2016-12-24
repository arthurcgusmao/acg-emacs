(require 'company)
(diminish 'company-mode)

;; enable company in all modes
(add-hook 'after-init-hook 'global-company-mode)


;; decrease delay before autocompletion popup shows
(setq company-idle-delay nil) ;; before: .3

;; do not require that the typed char when completing be a match
(setq company-require-match nil)

;; decrease delay before showing quickhelp
(setq company-quickhelp-delay 0.3)

;; setting keybindings
(define-key company-active-map (kbd "M-h") #'company-quickhelp-mode)
(define-key company-active-map (kbd "<escape>") #'company-abort)
(define-key company-mode-map (kbd "<tab>") 'company-indent-or-complete-common)


;; more company backends
;; (to do)
