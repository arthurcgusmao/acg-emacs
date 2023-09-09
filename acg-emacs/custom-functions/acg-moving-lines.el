;; utility to add final newline when executing a command

(defun acg/ensure-final-newline (&optional buffer)
  "Adds a final newline to a buffer if missing."
  (and (> (point-max) (point-min))
       (/= (char-after (1- (point-max))) ?\n)
       (not (and (eq selective-display t)
	         (= (char-after (1- (point-max))) ?\r)))
       (not buffer-read-only)
       (save-excursion
         (goto-char (point-max))
         (ignore-errors (insert "\n")))))


;; move the line(s) spanned by the active region up/down (line transposing)

(defun acg/move-lines (n)
  (let ((beg) (end) (keep))
    (if mark-active
        (save-excursion
          (setq keep t)
          (setq beg (region-beginning)
                end (region-end))
          (goto-char beg)
          (setq beg (line-beginning-position))
          (goto-char end)
          (setq end (line-beginning-position 2)))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))
    (let ((offset (if (and (mark t)
                           (and (>= (mark t) beg)
                                (< (mark t) end)))
                      (- (point) (mark t))))
          (rewind (- end (point))))
      (goto-char (if (< n 0) beg end))
      (forward-line n)
      (insert (delete-and-extract-region beg end))
      (backward-char rewind)
      (if offset (set-mark (- (point) offset))))
    (if keep
        (setq mark-active t
              deactivate-mark nil))))

(defun acg/move-lines-up (n)
  "move the line(s) spanned by the active region up by N lines."
  (interactive "*p")
  (unless (eq last-command this-command)
    (acg/ensure-final-newline))         ; Ensure final newline to prevent line clumping
  (acg/move-lines (- (or n 1))))

(defun acg/move-lines-down (n)
  "move the line(s) spanned by the active region down by N lines."
  (interactive "*p")
  (unless (eq last-command this-command)
    (acg/ensure-final-newline))         ; Ensure final newline to prevent line clumping
  (acg/move-lines (or n 1)))

;; binding
(global-set-key (kbd "<s-up>") 'acg/move-lines-up)
(global-set-key (kbd "<s-down>") 'acg/move-lines-down)
