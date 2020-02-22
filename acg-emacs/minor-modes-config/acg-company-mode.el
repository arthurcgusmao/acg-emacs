(use-package company)
(use-package company-quickhelp)
(use-package crux)
(use-package counsel)

;; disable on some modes
(setq company-global-modes '(not magit-mode
                                 magit-file-section
                                 magit-status-mode
                                 magit-log-mode
                                 dired-mode
                                 ))
;; enable company in all modes except the ones above
(add-hook 'after-init-hook 'global-company-mode)


;; decrease delay before autocompletion popup shows
(setq company-idle-delay nil) ;; before: .3

;; do not require that the typed char when completing be a match
(setq company-require-match nil)

;; decrease delay before showing quickhelp
(setq company-quickhelp-delay 0.3)

;; more company backends
;; (to do)


;; my custom function for custom indentation

(defun acg/company-indent-or-complete-common ()
  "Indent the current line or region (using function supplied as argument), or
complete the common part."
  (interactive)
  (cond
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((let ((old-point (point))
          (old-tick (buffer-chars-modified-tick))
          (tab-always-indent t))
      (call-interactively #'indent-for-tab-command)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick))
                 ;; do not run complete-common when blank chars before is
                 (not (or (eq (char-before) 10)                         ; newline
                          (eq (char-before) 32)                         ; whitespace
                          (eq (char-before) 41)                         ; )
                          (eq (char-before) 93)                         ; ]
                          (eq (char-before) 125))))                     ; }
        (company-complete-common)
        ;; (counsel-company)
        )))))

;; keybindings
(define-key company-active-map (kbd "M-h") #'company-quickhelp-mode)
(define-key company-active-map (kbd "<escape>") #'company-abort)
(define-key company-mode-map (kbd "<tab>") #'acg/company-indent-or-complete-common)
