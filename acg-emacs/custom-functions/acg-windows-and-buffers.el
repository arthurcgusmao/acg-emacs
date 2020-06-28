(use-package window
  :straight nil
  :init
  (defun acg/split-window-right ()
    "Same as `split-window-right' but runs `other-window' afterwards."
    (interactive)
    (split-window-right)
    (other-window 1)
    (other-buffer))

  (defun acg/split-window-below ()
    "Same as `split-window-below' but runs `other-window' afterwards."
    (interactive)
    (split-window-below)
    (other-window 1)
    (other-buffer))


  (defun acg/kill-buffer-and-window ()
    "Same as `kill-buffer-and-window' but acts differently when in
*Messages* buffer."
    (interactive)
    (if (equal (buffer-name (current-buffer)) "*Messages*")
        (next-buffer)
      (kill-buffer-and-window)))

  (defun acg/kill-buffer ()
    "Same as `kill-buffer-and-window' but acts differently when in
*Messages* buffer."
    (interactive)
    (if (equal (buffer-name (current-buffer)) "*Messages*")
        (next-buffer)
      (kill-buffer)))

  ;; Keybindings
  :bind
  (("M-1" . other-window)

   ("C-1" . delete-other-windows)
   ("C-!" . delete-window)
   ("C-2" . acg/split-window-right)
   ("C-@" . acg/split-window-below)

   ("C-q" . delete-window)

   ;; (acg/force-global-set-key (kbd "C-w") 'acg/kill-buffer)
   ("C-w" . acg/kill-buffer)))



(use-package crux
  :bind
  (("M-q" . crux-switch-to-previous-buffer)))



(use-package files
  :straight nil
  :config

  ;; From https://emacs.stackexchange.com/questions/3330/how-to-reopen-just-killed-buffer-like-c-s-t-in-firefox-browser
  ;; Make C-S-T reopen last closed buffer as in chrome

  (defvar acg/killed-file-list nil
    "List of recently killed files.")

  (defun acg/add-file-to-killed-file-list ()
    "If buffer is associated with a file name, add that file to
the `killed-file-list' when killing the buffer."
    (when buffer-file-name
      (push buffer-file-name acg/killed-file-list)))

  (add-hook 'kill-buffer-hook #'acg/add-file-to-killed-file-list)

  (defun acg/reopen-killed-file ()
    "Reopen the most recently killed file, if one exists."
    (interactive)
    (when acg/killed-file-list
      (find-file (pop acg/killed-file-list))))


  ;; "Save as" function

  (defun acg/save-buffer-as (filename)
    "Save buffer content as another file and open the file in a
new buffer."
    (interactive "FSave current buffer as: ")
    (write-region (point-min) (point-max) filename)
    (find-file filename))

  :bind
  (("C-S-T" . acg/reopen-killed-file)
   ("C-s" . save-buffer)
   ("C-S-s" . acg/save-buffer-as)))



;; Move buffers across windows
(use-package buffer-move
  :straight (:host github :repo "raxod502/buffer-move")
  :config
  (setq buffer-move-behavior 'move)
  :bind
  ("<M-s-up>" . buf-move-up)
  ("<M-s-down>" . buf-move-down)
  ("<M-s-left>" . buf-move-left)
  ("<M-s-right>" . buf-move-right))
