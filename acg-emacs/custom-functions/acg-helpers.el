;; only helper functions in this buffer.

(defun acg/current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun acg/current-indentation-column-p ()
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun acg/select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))


(defun acg/get-file-size (filename)
  "Returns the size of the file in bytes. See
https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Attributes.html"
  (nth 7 (file-attributes filename)))


(defun acg/parent-directory (dir)
  "Get parent directory of given path. Works for both files and dirs."
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun acg/recursively-make-directory (dir)
  "Create directories recursively, i.e., also create all parent
directories necessary."
  (unless (file-exists-p dir)
    (acg/recursively-make-directory (acg/parent-directory dir))
    (make-directory dir)))


(require 'multiple-cursors)
(defun kmacro-insert-letter (DELTA)
  "Similar to `kmacro-insert-counter', but inserts a letter
instead. DELTA argument can be passed to modify the increment
between each call to the function. If you want to start from a
letter different than 'a', call `kmacro-set-counter' before
starting to record the macro and change its value to the number
that corresponds to the respective letter offset (e.g., 0 -> a, 1
-> b, etc.)"
  (interactive "P")
  (mc/insert-letters kmacro-counter)
  (kmacro-add-counter (or DELTA 1)))
;; Set keybinding
(global-set-key (kbd "<S-f3>") 'kmacro-insert-letter)


;; Word mode helpers (subword and superword modes)

(defun acg/get-word-mode ()
  "Returns the current active word mode (superword or subword) or
nil if no word mode active."
  (cond
   (superword-mode 'superword-mode)
   (subword-mode 'subword-mode)))

(defun acg/reset-word-mode (original-word-mode altered-word-mode)
  "Restores the word mode to an original state, given that it has
been temporarily modified to an altered word mode."
  (unless (eq original-word-mode altered-word-mode)
    (if original-word-mode
        (set original-word-mode 1)
      (set altered-word-mode 0))))

(defun acg/with-subword-mode (fun)
  "Returns a function that executes the same command as `fun',
but on subword-mode. Does not depend on previous subword-mode
activation; original word mode is restored automatically."
  `(lambda (&optional arg)
     "Calls FUN on subword-mode. See `acg/with-subword-mode'"
     (interactive "^p")
     (setq arg (or arg 1))
     (with-current-buffer (current-buffer)
       (let ((word-mode (acg/get-word-mode)))
         (subword-mode 1)
         (,fun arg)
         (acg/reset-word-mode word-mode 'subword-mode)))))


;; Keychord helpers

(defun acg/set-transient-key (key command)
  "Set a transient keybinding that can be captured only in the
next keypress."
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd key) command)
    (set-transient-map kmap)))


;; Select minibuffer input

(defun acg/with-marked-input (&rest args)
  "Mark input of minibuffer. To be used as advice before any
function that starts with an initial input in the minibuffer."
  (run-with-idle-timer
   0 nil (lambda ()
           (push 'C-S-right unread-command-events)
           (push 'C-left unread-command-events))))