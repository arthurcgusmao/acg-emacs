(defun acg-kill-line-or-region ()
  "Kill line (or lines defined by the region) and adjust the indentation."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)))
  (progn
    (if (not (looking-at "[[:space:]]*$"))
        (kill-line))))     

(defun acg-kill-line-or-region-backwards ()
  "Kill line backwards (or lines defined by the region) and adjust the
indentation."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)))
  (kill-line 0)
  (indent-according-to-mode))

(require 'crux)
(defun acg-kill-whole-line-or-region-content ()
  "Kill all characters in line and indent, or kill line
if there are only white spaces in it."
  (interactive)
  (end-of-line)
  (crux-kill-line-backwards))

(defun acg-kill-whole-line-or-region-lines ()
  "Kills the whole line (or lines defined by the region)."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)))
  (kill-whole-line)
  (back-to-indentation))

(defun acg-kill-whole-line-or-region-lines-and-move-up ()
  "Kills the whole line (or lines defined by the region) and move up."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end)))
  (kill-whole-line)
  (previous-line)
  (back-to-indentation))

;; helper function
;; (defun current-line-empty-p ()
;;   "Checks if line is empty (has only whitespace characters)"
;;   (save-excursion
;;     (beginning-of-line)
;;     (looking-at "[[:space:]]*$")))

;; bindings
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)
(global-set-key (kbd "<C-S-backspace>") 'acg-kill-line-or-region-backwards)
(global-set-key (kbd "<M-backspace>") 'backward-kill-sexp)
(global-set-key (kbd "<C-S-delete>") 'acg-kill-line-or-region)
(global-set-key (kbd "<M-delete>") 'kill-sexp)
(global-set-key (kbd "C-e") 'acg-kill-whole-line-or-region-lines)
(global-set-key (kbd "C-S-e") 'acg-kill-whole-line-or-region-lines-and-move-up)
(global-set-key (kbd "M-e") 'acg-kill-whole-line-or-region-content)

