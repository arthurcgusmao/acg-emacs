(defun acg/expand-region-to-whole-lines ()
  "Expands the region to include the whole lines that it spans.

Adapted from https://spwhitton.name/blog/entry/expandregionlines/"
  (interactive)
  (if (use-region-p)
      (if (< (point) (mark))
          (let ((beg (point)))
            (goto-char (mark))
            (end-of-line)
            (push-mark)
            (goto-char beg)
            (beginning-of-line))
        (let ((end (point)))
          (goto-char (mark))
          (beginning-of-line)
          (push-mark)
          (goto-char end)
          (end-of-line)))
    ;; Region inactive - mark current line
    (beginning-of-line)
    (push-mark)
    (end-of-line)
    (activate-mark)))

(defun acg/get-region-unindented (pad beginning end)
  "Copy the region, un-indented by the length of its minimum indent.

If numeric prefix argument PAD is supplied, indent the resulting
text by that amount.

From https://emacs.stackexchange.com/a/34981/13589"
  (interactive "P\nr")
  (let ((buf (current-buffer))
        (itm indent-tabs-mode)
        (tw tab-width)
        (st (syntax-table))
        (indent nil))
    (with-temp-buffer
      (setq indent-tabs-mode itm
            tab-width tw)
      (set-syntax-table st)
      (insert-buffer-substring buf beginning end)
      ;; Establish the minimum level of indentation.
      (goto-char (point-min))
      (while (and (re-search-forward "^[[:space:]\n]*" nil :noerror)
                  (not (eobp)))
        (let ((length (current-column)))
          (when (or (not indent) (< length indent))
            (setq indent length)))
        (forward-line 1))
      (if (not indent)
          (error "Region is entirely whitespace")
        ;; Un-indent the buffer contents by the length of the minimum
        ;; indent level, and copy to the kill ring.
        (when pad
          (setq indent (- indent (prefix-numeric-value pad))))
        (indent-rigidly (point-min) (point-max) (- indent))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun acg/region-inside-defun-p ()
  "Returns t if region is inside a defun; nil otherwise."
  (let ((beg (region-beginning))
        (end (region-end)))
    (save-mark-and-excursion
      (er/mark-defun)
      (if (and (>= beg (region-beginning))
               (<= end (region-end)))
          t nil))))

(defun acg/mark-dwim ()
  "Mark, do what I mean."
  (interactive)
  (unless (use-region-p)
    (er/mark-paragraph)
    (if (acg/region-inside-defun-p)
        (er/mark-defun))))

(defun acg/mark-defun-body ()
  "Marks the body of a function definition by skipping the first
line."
  (interactive)
    (er/mark-defun)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (forward-line))


(defun acg/eval-with (eval-func mark-func)
  "Creates and returns a function that evaluates the region
marked by MARK-FUNC using EVAL-FUNC. If universal argment is
passed, evaluates the region only up to the cursor position."
  (let ((out-func-symbol
         (make-symbol
          (concat "acg/" (symbol-name eval-func)
                  "--" (symbol-name mark-func)))))
    (eval `(defun ,out-func-symbol (&optional arg)
             "Evaluates the lines of the region marked by the
respective function. If universal argument is passed, evaluates
the region only up to the line where the cursor is."
             (interactive "P")
             (save-mark-and-excursion
               (if arg
                   (progn
                     (save-excursion
                       (,mark-func)
                       (if (< (point) (mark))
                           (exchange-point-and-mark)))
                     (end-of-line))
                 (,mark-func))
               (,eval-func
                (region-beginning) (region-end)))))))
