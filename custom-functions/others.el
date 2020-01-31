
(defun seek-backward-to-char (chr)
  "Seek backwards to a character"
  (interactive "cSeek back to char: ")
  (while (not (= (char-after) chr))
    (forward-char -1)))


;; concentration mode
(defun acg/concentration-mode ()
  "Changes the window fringes to center text and limit distractions."
  (interactive)
  (set-window-fringes nil 500 0 t))
(global-set-key (kbd "<f12>") 'acg/concentration-mode)
