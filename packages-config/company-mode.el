(require 'company)
(diminish 'company-mode)

;; enable company in all modes
(add-hook 'after-init-hook 'global-company-mode)

;; using popups instead of opening another window for documentation
;; (company-quickhelp-mode 1)
;; make the documentation popup only appear when manually selected
;; (setq company-quickhelp-delay nil)

;; decrease delay before autocompletion popup shows
(setq company-idle-delay nil) ;; before: .3


;; setting keybindings
(define-key company-active-map (kbd "M-h") #'company-quickhelp-mode)
(define-key company-active-map (kbd "<escape>") #'company-abort)
(global-set-key (kbd "<tab>") 'company-complete)



;; more company backends
;; (to do)
