(defvar acg/scratch-buffer nil
  "Indicates whether buffer is a scratch buffer.")

;; (defun acg/scratch-buffer-toggle-major-mode ()
;;   (when (and acg/scratch-buffer
;;              ;; (= buffer-size 0)
;;              t)
;;     ))

(defun acg/scratch-buffer-create ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2016-08-11"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    (make-variable-buffer-local 'acg/scratch-buffer)
    ;; Flag variable should not be deleted when major mode changes
    (put 'acg/scratch-buffer 'permanent-local t)
    (setq acg/scratch-buffer t)))

(defun acg/scratch-buffer-unflag (&optional buffer)
  "Unset buffer as scratch buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (setq acg/scratch-buffer nil))))
(add-hook 'after-save-hook 'acg/scratch-buffer-unflag)

(defun acg/scratch-buffer-p (&optional buffer)
  "Indicates whether buffer is a scratch buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      acg/scratch-buffer)))

(defun acg/scratch-buffer-modified-p (&optional buffer)
  "Indicates if buffer is scratch and has been modified."
  (let ((buffer (or buffer (current-buffer))))
    (and (acg/scratch-buffer-p buffer)
         (buffer-modified-p buffer))))

(defun acg/scratch-buffer-kill-query-function ()
  (if (acg/scratch-buffer-modified-p)
      (yes-or-no-p "Scratch buffer modified; kill anyway? ")
    t))


;; Making backups of unsaved scratch buffers
(defun acg/scratch-buffer-save-backup ()
  "Write the contents of *scratch* to the file name
  PERSISTENT-SCRATCH-FILENAME, making a backup copy in
  PERSISTENT-SCRATCH-BACKUP-DIRECTORY."
  (let ((buffers (buffer-list)))
    (while buffers
      (with-current-buffer (get-buffer (car buffers))
        (if (acg/scratch-buffer-modified-p)
            (write-file (concat
                         acg/scratch-backup-dir
                         (format-time-string "%Y-%m-%d--%Hh%Mm%Ss--")
                         (buffer-name)))
          t))
      (setq buffers (cdr buffers)))))


;; Configuring and initializing

;; Run query before killing if buffer is acg/scratch-buffer
(add-to-list 'kill-buffer-query-functions 'acg/scratch-buffer-kill-query-function)
;; Adds the hook to be run whenever emacs is killed
(push #'acg/scratch-buffer-save-backup kill-emacs-hook)

;; Keybindings
(global-unset-key (kbd "C-n"))
(global-set-key (kbd "C-n") 'acg/scratch-buffer-create)


;; Remove the default scratch buffer

(defun acg/initial-buffer-choice ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*"))
  (get-buffer "*Messages*"))

(setq initial-buffer-choice 'acg/initial-buffer-choice)
