;; Restore cursor position after any command in a set of predefined commands
;; (configurable in `acg/cursor-ring-commands') is executed and `keyboard-quit' or
;; similar functions are called afterwards.

(defvar-local acg/cursor-ring nil "List of former cursor
positions of the current buffer, most recent first.")
(put 'acg/cursor-ring 'permanent-local t)

(defvar acg/cursor-ring-max 128 "Maximum size of cursor ring.
Start discarding off end if gets this big.")

(defvar acg/cursor-ring-commands
  '(beginning-of-buffer
    end-of-buffer
    mark-defun
    mark-page
    mark-paragraph
    mark-sexp
    mark-whole-buffer
    mark-work
    mwheel-scroll
    scroll-bar-drag
    scroll-bar-scroll-down
    scroll-bar-scroll-up
    scroll-bar-toolkit-scroll
    scroll-down-command
    scroll-other-window
    scroll-other-window-down
    scroll-up scroll-down
    scroll-up-command)
  "List of commands for which the cursor position will be pushed
to `acg/cursor-ring' before being called.")

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

;; Pre-command function
(defun acg/restore-cursor-pre-command ()
  (when (and (not (eq this-command last-command))
             (memq this-command acg/cursor-ring-commands)
             (not (memq last-command acg/cursor-ring-commands)))
    (acg/push-cursor-ring)))
(add-hook 'pre-command-hook 'acg/restore-cursor-pre-command)


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


;; ----------------------------------------
;; CONFIGS

(dolist (f '(acg/mark-dwim
             er/mark-defun))
  (add-to-list 'acg/cursor-ring-commands f))
