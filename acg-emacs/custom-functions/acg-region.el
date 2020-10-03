(defun acg/expand-region-to-whole-lines ()
  "Expands the region to include the whole lines that it spans.

Adapted from https://spwhitton.name/blog/entry/expandregionlines/"
  (interactive)
  (if (use-region-p)
      (if (< (point) (mark))
          (let ((beg (point)))
            (goto-char (mark))
            (end-of-line)
            (push-mark nil t)
            (goto-char beg)
            (beginning-of-line))
        (let ((end (point)))
          (goto-char (mark))
          (beginning-of-line)
          (push-mark nil t)
          (goto-char end)
          (end-of-line)))
    ;; Region inactive - mark current line
    (beginning-of-line)
    (push-mark nil t)
    (end-of-line)
    (activate-mark)))

(defun acg/with-expanded-region-to-whole-lines (orig-fun &rest args)
  "Run function but expanding region to whole lines, then restore
excursion. To be used as advice AROUND any function that acts
upon the selected region."
  (save-mark-and-excursion
    (acg/expand-region-to-whole-lines)
    (funcall orig-fun (region-beginning) (region-end))))

(defun acg/unindent-string (str &optional pad)
  "Returns a modified version of STRING by unindenting it by the
length of its minimum indent.

If optional numeric argument PAD is supplied, indent the
resulting text by that amount.

Adapted from https://emacs.stackexchange.com/a/34981/13589"
  (interactive "P\nr")
  (let ((itm indent-tabs-mode)
        (tw tab-width)
        (st (syntax-table))
        (indent nil))
    (with-temp-buffer
      (setq indent-tabs-mode itm
            tab-width tw)
      (set-syntax-table st)
      (insert str)
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


(defun acg/eval-with (eval-func mark-func &optional str-modif-func)
  "Creates and returns a function that evaluates the region
marked by MARK-FUNC using EVAL-FUNC.

If optional argument STR-MODIF-FUNC is nil, the marked region is
evaluated as-is, and EVAL-FUNC is expected to receive two
arguments: the values of `(region-beginning)' and `(region-end)'.
If STR-MODIF-FUNC is a function, it is applied to the buffer
substring of the region marked by MARK-FUNC before calling
EVAL-FUNC (i.e., STR-MODIF-FUNC takes a string as argument, and
returns another string). In this special case, EVAL-FUNC is
expected to receive a string (containing the code to be
evaluated) as argument, instead of the region positions.

See docstring of the returned function for its details.
"
  (let ((out-func-symbol
         (make-symbol
          (concat "acg/" (symbol-name eval-func)
                  "--" (symbol-name mark-func)
                  (if str-modif-func
                      (concat "--"
                              (symbol-name str-modif-func)))))))
    (eval `(defun ,out-func-symbol (&optional arg)
             "Evaluates the region marked by the respective
function. If universal argument is passed, evaluates the region
only up to the line where the cursor is. This function was
created by `acg/eval-with'."
             (interactive "P")
             (let (BEG END)
               (save-mark-and-excursion
                 (if arg
                     (progn
                       (save-excursion
                         (,mark-func)
                         (if (< (point) (mark))
                             (exchange-point-and-mark)))
                       (end-of-line))
                   (,mark-func))
                 (setq BEG (region-beginning)
                       END (region-end)))
               (if (quote ,str-modif-func)
                   (,eval-func
                    (,str-modif-func (buffer-substring BEG END))
                    BEG END)
                 (,eval-func BEG END)))))))


(defun acg/call-process-region
    (program args force-region whole-lines region-fun)
  "Call PROGRAM on the region marked by REGION-FUN,
replacing the region contents with the output.

A wrapper around `call-process-region' that better handles error
and point position. Improvements: raise an error when there is an
error in the child process, instead of deleting the buffer
contents; preserve position of point both after running the
command and when undo.

PROGRAM and ARGS are the same as in `call-process-region'.

If FORCE-REGION is non-nil, calls REGION-FUN even if region is
active. If WHOLE-LINES is non-nil, run
`acg/expand-region-to-whole-lines' after REGION-FUN.

REGION-FUN is a function that marks a region in the current
buffer. It is called inside `save-mark-and-excursion' to prevent
collateral effects, and receives no arguments."
  (interactive)
  (let ((buf (current-buffer))
        beg end)
    (save-mark-and-excursion
      (when (or force-region
                (not (region-active-p)))
        (funcall region-fun)
        (when whole-lines (acg/expand-region-to-whole-lines)))
      (setq beg (region-beginning)
            end (region-end)))
    (save-restriction
      (narrow-to-region beg end)
      (with-temp-buffer
        (insert-buffer buf)
        (apply 'call-process-region 1 (1+ (buffer-size))
               program t t nil args)
        (let ((temp-buf (current-buffer)))
          (with-current-buffer buf
            (replace-buffer-contents temp-buf)))))))

;; --------------------------------------------------------------
;; CONFIGS

;; Make sort-lines operate on the whole lines of the region, instead of trying
;; to operate on the region (which makes half-lines text from mid-selected
;; lines get cluttered with other lines)
(defadvice sort-lines (around advice-sort-lines activate)
  (interactive "P\nr")
  (acg/expand-region-to-whole-lines)
  (if (called-interactively-p 'any)
      (progn
        (call-interactively (ad-get-orig-definition 'sort-lines)))
    ad-do-it))
