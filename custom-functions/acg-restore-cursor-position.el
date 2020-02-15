;; Restore cursor position after any command in a set of predefined commands
;; (configurable in `acg/cursor-ring-commands') is executed and `keyboard-quit' or
;; similar functions are called afterwards.

(defvar-local acg/cursor-ring nil "List of former cursor
positions of the current buffer, most recent first.")
(put 'acg/cursor-ring 'permanent-local t)

(defvar acg/cursor-ring-max 128 "Maximum size of cursor ring.
Start discarding off end if gets this big.")

(defvar acg/cursor-ring-commands nil "List of commands for
which the cursor position will be pushed to `acg/cursor-ring'
before being called.")

(defun acg/push-cursor-ring ()
  "Pushes the current cursor position to the cursor ring if the
previous command was not in `acg/cursor-ring-commands'."
  (interactive)
  ;; Add current cursor position to ring
  (setq acg/cursor-ring (cons (copy-marker (point-marker)) acg/cursor-ring))
  ;; Discard oldest elements if ring limit has been exceeded
  (when (> (length acg/cursor-ring) acg/cursor-ring-max)
    (move-marker (car (nthcdr acg/cursor-ring-max acg/cursor-ring)) nil)
    (setcdr (nthcdr (1- acg/cursor-ring-max) acg/cursor-ring) nil)))

(defun acg/restore-cursor-position ()
  "Restores the most recent cursor position."
  (interactive)
  (goto-char (nth 0 acg/cursor-ring))
  (recenter-top-bottom))

(setq acg/cursor-ring-commands
      '(
        acg/mark-dwim
        er/mark-defun
        mark-defun
        mark-page
        mark-paragraph
        mark-whole-buffer
        scroll-down-command
        scroll-up-command
        ;; forward-paragraph
        ;; backward-paragraph
        end-of-buffer
        beginning-of-buffer
        ;; mwheel-scroll ; Not working @TODO
        ))

;; Add advice to all functions in list
;; Note: couldn't use `advice-add' because it wasn't handling the "^p" and "^P"
;; interactive cases (shift handling was not happening).
(dolist (f acg/cursor-ring-commands)
  (eval
   `(defadvice ,f (around acg/push-cursor-ring-adv activate)
      (interactive)
      (unless (memq last-command acg/cursor-ring-commands)
        (acg/push-cursor-ring))
      (if (called-interactively-p 'interactive)
          (call-interactively (ad-get-orig-definition ',f))
        ad-do-it))))


;; Advise functions that perform quit to restore cursor position

(defadvice keyboard-quit (before acg/restore-cursor activate)
  (when (memq last-command acg/cursor-ring-commands)
    (acg/restore-cursor-position)))

(defadvice minibuffer-keyboard-quit (around acg/restore-cursor activate)
  (if (memq last-command acg/cursor-ring-commands)
      (acg/restore-cursor-position)
    ad-do-it))

(defadvice cua-cancel (before acg/restore-cursor activate)
  (when (memq last-command acg/cursor-ring-commands)
    (acg/restore-cursor-position)))


