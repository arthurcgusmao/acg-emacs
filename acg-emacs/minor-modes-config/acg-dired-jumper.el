;; (completing-read "Dired quick jump: ")


;; (read-from-minibuffer "wooooo ")

;; (dired-goto-file )



;; (defun dired-jumper)
;; post-self-insert-hook




;; 
;; (defvar dired-jumper--default-directory nil
;;   "Store `default-directory' of the dired buffer where
;; `dired-jumper' was activated.")

;; (defvar dired-jumper--dired-buffer nil
;;   "Store `default-directory' of the dired buffer where
;; `dired-jumper' was activated.")


;; (defun dired-jumper--post-insert ()
;;   "Function run after `post-self-insert-hook'. Moves point to a
;; file that matches the typed text."
;;   (let* ((search-str (minibuffer-contents))
;;          (filepath (concat dired-jumper--default-directory search-str)))
;;     (with-current-buffer dired-jumper--dired-buffer
;;       (dired-goto-file filepath))))


;; ;;;###autoload
;; (define-minor-mode dired-jumper-mode
;;   "Mode for quickly jumping to files in dired."
;;   :global nil
;;   (if dired-jumper-mode
;;       (progn
;;         (unless (derived-mode-p 'dired-mode)
;;           (error "`dired-jumper' should only be activated from a dired buffer."))
;;         (setq dired-jumper--default-directory default-directory)
;;         (setq dired-jumper--dired-buffer (current-buffer))
;;         (add-hook 'post-self-insert-hook 'dired-jumper--post-insert)
;;         (read-from-minibuffer "Dired jumper: ")
;;         )
;;     (progn
;;       (remove-hook 'post-self-insert-hook 'dired-jumper--post-insert))))

;; (provide 'dired-jumper)
;; ;;; restore-point.el ends here



