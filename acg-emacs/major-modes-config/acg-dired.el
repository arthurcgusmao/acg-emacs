(use-package dired
  :straight nil
  :config
  (put 'dired-find-alternate-file 'disabled nil) ;; enable disabled command

  (defun acg/dired-alternate-up-directory (&optional other-window)
    "Run Dired on parent directory of current directory.
Taken from https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer"
    (interactive "P")
    (let* ((dir (dired-current-directory))
           (orig (current-buffer))
           (up (file-name-directory (directory-file-name dir))))
      (or (dired-goto-file (directory-file-name dir))
          ;; Only try dired-goto-subdir if buffer has more than one dir.
          (and (cdr dired-subdir-alist)
               (dired-goto-subdir up))
          (progn
            (kill-buffer orig)
            (dired up)
            (dired-goto-file dir)))))

  (defun acg/dired-mouse-find-alternate-file (event &optional find-dir-func)
    "Calls `dired-mouse-find-file' with FIND-FILE-FUNC equals to
    `find-alternate-file'."
    (interactive "e")
     (let ((orig (current-buffer)))
       (dired-mouse-find-file event #'find-alternate-file find-dir-func)
       (kill-buffer orig)))

  (defun acg/dired-display-file-and-next ()
    "Same as `dired-display-file' but moves point to next file
afterwards."
    (interactive)
    (dired-display-file)
    (dired-hacks-next-file))

  (defun acg/dired-display-file-and-previous ()
    "Same as `dired-display-file' but moves point to previous file
afterwards."
    (interactive)
    (dired-display-file)
    (dired-hacks-previous-file))

  ;; Group directories together
  (unless (string-equal system-type "darwin") ; Doesn't work on Mac without root permissions
    (setq dired-listing-switches "--group-directories-first -al"))

  :bind
  (("C-x d" . dired-jump)
   ("C-M-d" . dired-jump)
   :map dired-mode-map
   ("C-o" . nil)
   ("q" . 'kill-current-buffer)
   ("<return>" . 'dired-find-alternate-file)
   ;; ("<C-return>" . 'dired-find-file)
   ("<C-return>" . 'dired-display-file)
   ("<S-return>" . 'acg/dired-display-file-and-next)
   ("<C-S-return>" . 'acg/dired-display-file-and-previous)
   ("<backspace>" . 'acg/dired-alternate-up-directory)
   ("<C-backspace>" . 'dired-up-directory)
   ([C-mouse-2] . 'dired-mouse-find-file-other-window) ;; not working @todo
   ([mouse-2] . 'acg/dired-mouse-find-alternate-file)))

(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
   ("<tab>" . 'dired-subtree-cycle)
   ("<S-iso-lefttab>" . 'dired-subtree-remove)))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar))
