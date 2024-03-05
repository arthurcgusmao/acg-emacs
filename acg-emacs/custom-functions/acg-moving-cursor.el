;; Adapted from
;; https://www.reddit.com/r/emacs/comments/543ri3/beginningoflineorindentation/

(defun acg/point-at-indentation-p ()
  "Return non-nil if point is at indentation, nil otherwise."
  (= (save-excursion (back-to-indentation) (point)) (point)))

(defun acg/second-line-of-visual-line-p ()
  "Return non-nil if point is at the second or more line of a
  visual line."
  (let ((line-move-visual t))
    (= (save-excursion (line-move -1 t) (line-number-at-pos)) (line-number-at-pos))))

(defun acg/beginning-of-line-or-indentation (arg)
  "Toggle between beginning of line and point of indentation."
  (interactive "^p")
  (if (acg/point-at-indentation-p)
      (beginning-of-line)
    (back-to-indentation)))

(defun acg/beginning-of-visual-line-or-indentation (arg)
  "Toggle between beginning of visual line and point of indentation."
  (interactive "^p")
  (if (or (acg/point-at-indentation-p)
          (acg/second-line-of-visual-line-p))
      (beginning-of-visual-line)
    (back-to-indentation)))

(fset 'acg/left-subword (acg/with-subword-mode #'backward-word))
(fset 'acg/right-subword (acg/with-subword-mode #'forward-word))

;; keybindings
(global-set-key (kbd "<home>") 'acg/beginning-of-visual-line-or-indentation)
(global-set-key (kbd "<C-left>") 'acg/beginning-of-visual-line-or-indentation)
(global-set-key (kbd "<C-right>") 'move-end-of-line)
(global-set-key (kbd "<s-left>") 'acg/left-subword)
(global-set-key (kbd "<s-right>") 'acg/right-subword)
