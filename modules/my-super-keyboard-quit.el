(defun my-super-keyboard-quit ()
  "Run many similar quit commands to make this function a general quit."
  (interactive)
  (keyboard-quit)
  (abort-recursive-edit)
  (setq quit-flag t)
  )
