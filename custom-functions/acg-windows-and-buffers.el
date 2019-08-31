(require 'crux)

(defun acg-split-window-right ()
  "Same as `split-window-right' but runs `other-window' afterwards."
  (interactive)
  (split-window-right)
  (other-window 1)
  (other-buffer))

(defun acg-split-window-below ()
  "Same as `split-window-below' but runs `other-window' afterwards."
  (interactive)
  (split-window-below)
  (other-window 1)
  (other-buffer))


(defun acg-kill-buffer-and-window ()
  "Same as `kill-buffer-and-window' but acts differently when in
*Messages* buffer."
  (interactive)
  (if (equal (buffer-name (current-buffer)) "*Messages*")
      (next-buffer)
    (kill-buffer-and-window)))


;; from https://emacs.stackexchange.com/questions/3330/how-to-reopen-just-killed-buffer-like-c-s-t-in-firefox-browser
;; making C-S-T reopen last closed buffer as in chrome

(defvar acg-killed-file-list nil
  "List of recently killed files.")

(defun acg-add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name acg-killed-file-list)))

(add-hook 'kill-buffer-hook #'acg-add-file-to-killed-file-list)

(defun acg-reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when acg-killed-file-list
    (find-file (pop acg-killed-file-list))))


;; indentify unmodified buffer

(defvar acg-check-buffer-modified-size-limit 50000
  "Size limit (in bytes) to try to check if buffer differs from
  saved file. Applies to both the saved file and the buffer: if
  either is larger than the specified amount, no check will be
  performed.")

(defvar acg-scheduled-buffer-timer nil
  "Stores the latest scheduled timer to be run to check if the
  buffer is in modified state (in comparison to its saved file).")
(make-variable-buffer-local 'acg-scheduled-buffer-timer)

;; useful for debugging, delete later:
;; (defvar count-run-times 0 "")
;; (setq count-run-times 0)
;; (message (concat "Times run: " (number-to-string count-run-times)))

(defun acg-update-modified-buffer-flag (buffer)
  "Update the buffer modified flag if content does not differ
from respective file in disk. Requires diff to be installed on
your system. Adapted from
https://stackoverflow.com/a/11452885/5103881"
  (interactive)
  (let ((basefile (buffer-file-name buffer)))
    (unless (not basefile) ;; buffer must be associated to a file
      (let ((b-size (buffer-size buffer))
            (f-size (acg-get-file-size basefile)))
        ;; (setq count-run-times (+ count-run-times 1)) ;; debugging purposes - delete later
        (unless (or (/= b-size f-size) ;; buffer size should be equal to file size (much faster comparison than diffing)
                    (> b-size acg-check-buffer-modified-size-limit) ;; buffer size must be smaller than limit
                    (> f-size acg-check-buffer-modified-size-limit)) ;; file size must be smaller than limit
          (let ((tempfile (make-temp-file "buffer-content-")))
            (with-current-buffer buffer
              (save-restriction
                (widen)
                (write-region (point-min) (point-max) tempfile nil 'silent))
              ;; unless buffer diffs from file
              (unless (/= (call-process "diff" nil nil nil "-q" basefile tempfile) 0) ;; returns 0 if files are equal, 1 if different, and 2 if invalid file paths
                (progn
                  (set-buffer-modified-p nil) ;; set unmodified state (important emacs native flag)
                  (ztl-modification-state-change)))) ;; update tabbar
            (delete-file tempfile)))))))

(defun acg-schedule-modified-buffer-update (beg end len)
  "Schedules a check of the actual buffer state (if it is really
different than its corresponding file in disk). Arguments are not
used but must comply with `after-chenge-functions' call."
  (combine-after-change-calls
    (with-current-buffer (current-buffer)
      (save-restriction
        (acg-cancel-schedule-modified-buffer-update) ;; delete previously scheduled timer
        (setq acg-scheduled-buffer-timer ;; schedule new timer and save requested action to variable
              (run-at-time "0.5 sec" nil #'acg-update-modified-buffer-flag (current-buffer))))))) ;; use 500 msec waiting time

(defun acg-cancel-schedule-modified-buffer-update ()
  "Cancels the last scheduling of a check of the actual buffer
state (if it is really different than its corresponding file in
disk)."
  (if (bound-and-true-p acg-scheduled-buffer-timer)
      (cancel-timer acg-scheduled-buffer-timer)))

;; Add after-change-hook locally for buffers who visit a file
(add-hook 'find-file-hook
          (lambda () (with-current-buffer (current-buffer)
                       (add-hook 'after-change-functions 'acg-schedule-modified-buffer-update nil t))))
;; No need to check status when file has been saved
(add-hook 'after-save-hook 'acg-cancel-schedule-modified-buffer-update)


;; keybindings

(global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "M-q") 'crux-switch-to-previous-buffer)

(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-!") 'delete-window)
(global-set-key (kbd "C-2") 'acg-split-window-right)
(global-set-key (kbd "C-@") 'acg-split-window-below)

(global-set-key (kbd "C-q") 'delete-window)

(acg-force-global-set-key (kbd "C-w") 'acg-kill-buffer-and-window)

(global-set-key (kbd "C-S-T") 'acg-reopen-killed-file)
