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

(require 'crux)
(defun acg/kill-whole-line-or-region-content ()
  "Kill all characters in line and indent, or kill line
if there are only white spaces in it."
  (interactive)
  (end-of-line)
  (crux-kill-line-backwards))

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
(acg/force-global-set-key "<C-backspace>" 'acg/backward-kill-word)
(global-set-key (kbd "<s-backspace>") (acg/with-subword-mode #'acg/backward-kill-word))
(acg/force-global-set-key "<M-backspace>" 'acg/backward-kill-sexp)

(acg/force-global-set-key "<M-delete>" 'acg/kill-sexp)
(acg/force-global-set-key "<s-delete>" (acg/with-subword-mode #'kill-word))
(global-set-key (kbd "<M-S-delete>") 'acg/kill-whole-line-or-region-lines-and-move-up)

(acg/force-global-set-key "<C-S-backspace>" 'acg/kill-line-or-region-backwards)
(acg/force-global-set-key "<C-S-delete>" 'acg/kill-line-or-region)

(global-set-key (kbd "<S-delete>") 'acg/kill-whole-line-or-region-lines)
(acg/force-global-set-key "<S-backspace>" 'acg/kill-whole-line-or-region-content)
