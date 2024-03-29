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
    (with-current-buffer -buf
      (funcall initial-major-mode)
      (setq buffer-offer-save nil)
      (make-variable-buffer-local 'acg/scratch-buffer)
      ;; Flag variable should not be deleted when major mode changes
      (put 'acg/scratch-buffer 'permanent-local t)
      (setq acg/scratch-buffer t))))

(defun acg/scratch-buffer-open-backup-dir ()
  "Open the directory containing persisted scratch buffers."
  (interactive)
  (find-file acg/scratch-backup-dir))

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
(defun acg/scratch-buffer-save-backup-for-all-buffers ()
  (let ((buffers (buffer-list)))
    (while buffers
      (with-current-buffer (get-buffer (car buffers))
        (acg/scratch-buffer-maybe-save-backup-for-current-buffer))
      (setq buffers (cdr buffers)))))

(defun acg/scratch-buffer-maybe-save-backup-for-current-buffer ()
  "Write the contents of *scratch* to the file name
  PERSISTENT-SCRATCH-FILENAME, making a backup copy in
  PERSISTENT-SCRATCH-BACKUP-DIRECTORY."
  (if (acg/scratch-buffer-modified-p)
      (write-file (concat
                   acg/scratch-backup-dir
                   (format-time-string "%Y-%m-%d--%Hh%Mm%Ss--")
                   (buffer-name)))
    t))


;; Configuring and initializing

;; Run query before killing if buffer is acg/scratch-buffer
;; (add-to-list 'kill-buffer-query-functions 'acg/scratch-buffer-kill-query-function)
;; Adds the hook to be run whenever emacs is killed
(push #'acg/scratch-buffer-save-backup-for-all-buffers kill-emacs-hook)
(add-hook 'kill-buffer-hook #'acg/scratch-buffer-maybe-save-backup-for-current-buffer)

;; Keybindings
(global-set-key (kbd "M-t") 'acg/scratch-buffer-create)


;; Remove the default scratch buffer

(defun acg/initial-buffer-choice ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*"))
  (get-buffer "*Messages*"))

(setq initial-buffer-choice 'acg/initial-buffer-choice)
