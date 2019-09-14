;; only helper functions in this buffer.

(defun acg-current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun acg-current-indentation-column-p ()
  (save-excursion
    (back-to-indentation)
    (current-column)))


(defun acg-get-file-size (filename)
  "Returns the size of the file in bytes. See
https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Attributes.html"
  (nth 7 (file-attributes filename)))


(defun acg-parent-directory (dir)
  "Get parent directory of given path. Works for both files and dirs."
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun acg-recursively-make-directory (dir)
  "Create directories recursively, i.e., also create all parent
directories necessary."
  (unless (file-exists-p dir)
    (acg-recursively-make-directory (acg-parent-directory dir))
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
