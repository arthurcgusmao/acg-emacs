(defun custom-scratch-buffer-create ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2016-08-11"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    (set (make-local-variable 'custom-scratch-buffer) t)))

(defun custom-scratch-buffer-kill-query-function ()
  (if (and (not buffer-file-name)   ;; buffer is not visiting a file
           (buffer-modified-p)      ;; buffer has been modified
	   (boundp 'custom-scratch-buffer))
      (if 'custom-scratch-buffer ;; buffer is a custom scratch created buffer
      	  (yes-or-no-p "Scratch buffer modified. Kill it anyway? "))
    t))


;;;;;;;;;;;;;;;
;; configuring and initializing
;;;;;;;;;;;;;;;

;; run query before killing if buffer is custom-scratch-buffer
(add-to-list 'kill-buffer-query-functions 'custom-scratch-buffer-kill-query-function)
