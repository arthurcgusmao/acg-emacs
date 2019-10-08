;; updated unmodified buffer

(defvar acg-check-buffer-modified-size-limit 50000
  "Size limit (in bytes) to try to check if buffer differs from
  saved file. Applies to both the saved file and the buffer: if
  either is larger than the specified amount, no check will be
  performed.")

(defvar acg-scheduled-buffer-timer nil
  "Stores the latest scheduled timer to be run to check if the
  buffer is in modified state (in comparison to its saved file).")
(make-variable-buffer-local 'acg-scheduled-buffer-timer)

(defvar acg-unmodified-buffer-hook nil
  "List of functions to be called when a buffer is set to unmodified.")

;; useful for debugging, delete later:
;; (defvar count-run-times 0 "")
;; (setq count-run-times 0)
;; (message (concat "Times run: " (number-to-string count-run-times)))

(defun acg-update-modified-buffer-flag (buffer)
  "Update the buffer modified flag if content does not differ
from respective file in disk. Requires diff to be installed on
your system. Adapted from
https://stackoverflow.com/a/11452885/5103881"
  (if (buffer-live-p buffer) ;; check that buffer has not been killed
      (let ((basefile (buffer-file-name buffer)))
        (unless (not basefile) ;; buffer must be associated to a file
          (let ((b-size (buffer-size buffer))
                (f-size (acg-get-file-size basefile)))
            ;; (setq count-run-times (+ count-run-times 1)) ;; debugging purposes - delete later
            (unless (or (not f-size) ; sometimes the buffer can be associated to a file but the file does not exist on disk. This line covers this case.
                        (/= b-size f-size) ;; buffer size should be equal to file size (much faster comparison than diffing)
                        (> b-size acg-check-buffer-modified-size-limit) ;; buffer size must be smaller than limit
                        (> f-size acg-check-buffer-modified-size-limit)) ;; file size must be smaller than limit
              (let ((tempfile (make-temp-file "buffer-content-")))
                (with-current-buffer buffer
                  (save-restriction
                    (widen)
                    (write-region (point-min) (point-max) tempfile nil 'silent))
                  ;; unless buffer diffs from file
                  (unless
                      (cond
                       ((string-equal system-type "windows-nt")
                        (/= (call-process "FC" nil nil nil "/B"
                                          (replace-regexp-in-string "/" "\\" basefile t t)
                                          (replace-regexp-in-string "/" "\\" tempfile t t)) 0))
                       ((string-equal system-type "darwin")
                        (message "Mac not supported. File a bug report or pull request."))
                       ((string-equal system-type "gnu/linux")
                        (/= (call-process "diff" nil nil nil "-q" basefile tempfile) 0))) ;; returns 0 if files are equal, 1 if different, and 2 if invalid file paths
                    (progn
                      (set-buffer-modified-p nil) ;; set unmodified state (important emacs native flag)
                      (--each acg-unmodified-buffer-hook (funcall it)))))
                (delete-file tempfile))))))))

(defun acg-schedule-modified-buffer-update (beg end len)
  "Schedules a check of the actual buffer state (if it is really
different than its corresponding file in disk). Arguments are not
used but must comply with `after-chenge-functions' call."
  (save-match-data ; necessary; otherwise running `replace-string' was giving error "Match data clobbered by buffer modification hooks."
    (with-current-buffer (current-buffer)
      (acg-cancel-schedule-modified-buffer-update) ;; delete previously scheduled timer
      (setq acg-scheduled-buffer-timer ;; schedule new timer and save requested action to variable
            (run-at-time "0.5 sec" nil #'acg-update-modified-buffer-flag (current-buffer)))))) ;; use 0.5 sec 'buffer' time

(defun acg-cancel-schedule-modified-buffer-update ()
  "Cancels the last scheduling of a check of the actual buffer
state (if it is really different than its corresponding file in
disk)."
  (if (bound-and-true-p acg-scheduled-buffer-timer)
      (cancel-timer acg-scheduled-buffer-timer)))

;; Add after-change-hook locally for buffers who visit a file
(add-hook 'find-file-hook
          (lambda () (with-current-buffer (current-buffer)
                       (add-hook 'after-change-functions 'acg-schedule-modified-buffer-update t t))))
;; No need to check status when file has been saved
(add-hook 'after-save-hook 'acg-cancel-schedule-modified-buffer-update)
