;; (require 'company)

;; (defun smart-tab-behavior ()
;;   "Tries to indent and correct cursor position first. If not possible, runs
;; company-complete."
;;   (interactive)
;;   (let ((line-end-relative-position (line-beginning-position 2)))
;;     ;; indent region or line
;;     ;;    (using the same principle of crux's with-region-or-line,
;;     ;;    but I couldn't get it working on here by calling the function)
;;     (if mark-active
;;         (indent-region (region-beginning) (region-end))
;;       (indent-region (line-beginning-position) (line-beginning-position 2)))
    
;;     ;; line has been indented
;;     (if (not (eq line-end-relative-position (line-beginning-position 2)))
;;         (if (point-before-indentation)
;;             (back-to-indentation)) ;; moves cursor if necessary
      
;;       ;; line has not been indented
;;       (if (point-before-indentation)
;;           (back-to-indentation) ;; moves cursor if necessary or
;;         (company-complete)
;;         ))))


;; ;; helper functions
;; (defun point-before-indentation ()
;;   "Goes back to indentation if the point position is before the first character
;; of the line. Returns cursor position if cursor was after or on indentation and 
;; nil if before."
;;   (let ((initial-pos (point)))
;;     (back-to-indentation)
;;     (let ((final-pos (point)))
;;       (goto-char initial-pos)
;;       (if (< initial-pos final-pos)
;;           t
;;         nil))))


;; (define-key company-mode-map (kbd "<tab>") 'smart-tab-behavior)
;; (global-set-key (kbd "<tab>") 'custom-tab-behavior)
;; (define-key global-set-key (kbd "<S-iso-lefttab>") 'custom-tab-behavior)
