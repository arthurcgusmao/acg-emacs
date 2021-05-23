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
      (let ((last-nonmenu-event nil)) ;; Forces dialog box
        (kill-buffer))))

  (defun acg/kill-all-normal-buffers ()
    "Kill all normal buffers (non-emacs buffers)."
    (dolist (cur (buffer-list))
      (if (not (equal (substring (buffer-name cur) 0 1) "*"))
          (kill-buffer cur))))

  (defun acg/save-some-buffers (&rest args)
    "Similar to `save-some-buffers', with enhancements: shows
buffer for each prompt, include scratch buffers in the prompt,
then redisplays the buffer where the function was issued."
    (interactive)
    (save-window-excursion
      ;; Display a single window to increase focus on the queried buffer
      (delete-other-windows)
      ;; First save any buffers that we're supposed to save
      ;; unconditionally.  That way the following code won't ask
      ;; about them.
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
	  (when (and buffer-save-without-query (buffer-modified-p))
	    (push (buffer-name) autosaved-buffers)
	    (save-buffer))))
      ;; Go through the list of "normal" buffers, displaying them and prompting
      ;; for save.
      (dolist (buffer (buffer-list))
        (and (buffer-live-p buffer)
	     (buffer-modified-p buffer)
             (not (buffer-base-buffer buffer))
             (or
              (buffer-file-name buffer)
              (with-current-buffer buffer
                (or (eq buffer-offer-save 'always)
                    (and buffer-offer-save
                         (> (buffer-size) 0)))))
             (display-buffer-same-window buffer '())
             ;; Prompt user for saving
             (let ((last-nonmenu-event nil)) ;; Forces dialog box
               (yes-or-no-p
                (format "  Save %s? " (buffer-name buffer))))
             ;; Save buffer
             (with-current-buffer buffer
               (save-buffer))))))

  (advice-add 'delete-frame :before #'acg/save-some-buffers)

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
  (("C-o" . find-file)
   ("C-S-T" . acg/reopen-killed-file)
   ("C-s" . save-buffer)
   ("C-S-s" . acg/save-buffer-as)))



;; Move buffers across windows
(use-package buffer-move
  :straight (:host github :repo "lukhas/buffer-move")
  :config
  (setq buffer-move-behavior 'move)
  :bind
  ("<M-s-up>" . buf-move-up)
  ("<M-s-down>" . buf-move-down)
  ("<M-s-left>" . buf-move-left)
  ("<M-s-right>" . buf-move-right))
