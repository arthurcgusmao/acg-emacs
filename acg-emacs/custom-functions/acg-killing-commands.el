(defun acg/kill-line-or-region ()
  "Kill line (or lines defined by the region) and adjust the indentation."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)))
  (progn
    (if (not (looking-at "[[:space:]]*$"))
        (kill-line))))

(defun acg/kill-line-or-region-backwards ()
  "Kill line backwards (or lines defined by the region) and adjust the
indentation."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)))
  (kill-line 0)
  (indent-according-to-mode))

(defun acg/toggle-line-indent ()
  "Toggles between indentation according to mode and no
indentation in the current line."
  (interactive)
  (let ((idnt (current-indentation)))
    (indent-according-to-mode)
    (when (= idnt (current-indentation))
      (indent-line-to 0))))

(defun acg/kill-whole-line-or-region-content ()
  "Kill all characters in line and indent, or kill line
if there are only white spaces in it."
  (interactive)
  (if (acg/line-empty-p)
      ;; Be smart: toggle between beginning of 'hard' line and previous line
      ;; indentation when line is already empty
      (acg/toggle-line-indent)
    (progn
      (if (use-region-p)
          (kill-region (region-beginning) (region-end)))
      (end-of-line)
      (kill-line 0)
      (indent-according-to-mode))))

(defun acg/kill-whole-line-or-region-lines ()
  "Kills the whole line (or lines defined by the region)."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)))
  (kill-whole-line)
  (back-to-indentation))

(defun acg/kill-whole-line-or-region-lines-and-move-up ()
  "Kills the whole line (or lines defined by the region) and move up."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)))
  (kill-whole-line)
  (previous-line)
  (back-to-indentation))

(defun acg/backward-delete-to-char (char)
  (interactive (list (read-char-from-minibuffer "Delete backward to char: "
						nil 'read-char-history)))
  (zap-up-to-char -1 char nil))

(defun acg/forward-delete-to-char (char)
  (interactive (list (read-char-from-minibuffer "Delete forward to char: "
						nil 'read-char-history)))
  (zap-up-to-char +1 char nil))

(defun acg/backward-kill-sexp ()
  "Same as `backward-kill-sexp' but kills region first if
active."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)))
  (backward-kill-sexp))

(defun acg/backward-kill-word (&optional arg)
  "Same as `backward-kill-word' but kills region first if
active."
  (interactive "^p")
  (setq arg (or arg 1))
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)))
  (backward-kill-word arg))

(defun acg/kill-sexp ()
  "Same as `kill-sexp' but kills region first if active."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)))
  (kill-sexp))

;; helper function
;; (defun current-line-empty-p ()
;;   "Checks if line is empty (has only whitespace characters)"
;;   (save-excursion
;;     (beginning-of-line)
;;     (looking-at "[[:space:]]*$")))

;; bindings
(acg/force-global-set-key "<M-backspace>" 'acg/backward-kill-word)
(global-set-key (kbd "<s-backspace>") (acg/with-subword-mode #'acg/backward-kill-word))
(acg/force-global-set-key "<C-backspace>" 'acg/backward-kill-sexp)

(acg/force-global-set-key "<C-delete>" 'acg/kill-sexp)
(acg/force-global-set-key "<s-delete>" (acg/with-subword-mode #'kill-word))
(acg/force-global-set-key "s-<kp-delete>" (acg/with-subword-mode #'kill-word)) ; For MacOS
(global-set-key (kbd "<C-S-delete>") 'acg/kill-whole-line-or-region-lines-and-move-up)

(acg/force-global-set-key "<M-S-backspace>" 'acg/kill-line-or-region-backwards)
(acg/force-global-set-key "<M-S-delete>" 'acg/kill-line-or-region)

(global-set-key (kbd "<S-delete>") 'acg/kill-whole-line-or-region-lines)
(acg/force-global-set-key "<S-backspace>" 'acg/kill-whole-line-or-region-content)
