(defvar acg-scratch-buffer-directory (concat acg-backup-dir "scratch-buffer-backups/")
  "Prefix path for scratch buffers")

(defun acg-scratch-buffer-create ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2016-08-11"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    (set (make-local-variable 'acg-scratch-buffer) t)))

(defun acg-scratch-buffer-kill-query-function ()
  (if (and (not buffer-file-name)   ;; buffer is not visiting a file
           (buffer-modified-p)      ;; buffer has been modified
	   (boundp 'acg-scratch-buffer))
      (if 'acg-scratch-buffer ;; buffer is an acg-scratch created buffer
      	  (yes-or-no-p "Scratch buffer modified. Kill it anyway? "))
    t))


;;;;;;;;;;;;;;;
;; making backup of unsaved acg-scratch buffers
;;;;;;;;;;;;;;;

(defun acg-scratch-buffer-save-backup ()
  "Write the contents of *scratch* to the file name
  PERSISTENT-SCRATCH-FILENAME, making a backup copy in
  PERSISTENT-SCRATCH-BACKUP-DIRECTORY."
  (let ((buffers (buffer-list)))
    (while buffers
      (with-current-buffer (get-buffer (car buffers))
        (if (and (not buffer-file-name)   ;; buffer is not visiting a file
                 (buffer-modified-p)      ;; buffer has been modified
                 (boundp 'acg-scratch-buffer))
            (if 'acg-scratch-buffer ;; buffer is a acg-scratch created buffer
                (write-file (concat
                             acg-scratch-buffer-directory
                             (format-time-string "%Y-%m-%d--%Hh%Mm%Ss--")
                             (buffer-name))))
          t))
      (setq buffers (cdr buffers)))))


;;;;;;;;;;;;;;;
;; configuring and initializing
;;;;;;;;;;;;;;;

;; run query before killing if buffer is acg-scratch-buffer
(add-to-list 'kill-buffer-query-functions 'acg-scratch-buffer-kill-query-function)
;; adds the hook to be run whenever emacs is killed
(push #'acg-scratch-buffer-save-backup kill-emacs-hook)


;; keybindings
(global-unset-key (kbd "C-n"))
(global-set-key (kbd "C-n") 'acg-scratch-buffer-create)
