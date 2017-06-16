
(defun seek-backward-to-char (chr)
  "Seek backwards to a character"
  (interactive "cSeek back to char: ")
  (while (not (= (char-after) chr))
    (forward-char -1)))


;; concentration mode:
;; create a function that changes the fringes width to make the file centralize when in full screen
;; then when the user press the key it goes fullscreen and automatically the fringes go thick
;; (set-window-fringes nil 500 500 t)
