(use-package dired
  :straight nil
  :config
  (put 'dired-find-alternate-file 'disabled nil) ;; enable disabled command

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  ;; When copying or moving files, automatically infer the destination
  ;; directory if another dired window is open.
  (setq dired-dwim-target t)

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

  ;; MacOS correction to use "GNU" ls instead of MacOS's ls -- from https://emacs.stackexchange.com/a/29101/13589
  (when (string-equal system-type "darwin")
    (setq insert-directory-program "gls"))

  ;; Group directories together
  (setq dired-listing-switches "--group-directories-first -al")

  ;;; Other utility functions
  (defun prot-dired-limit-regexp (regexp omit)
    "Limit Dired to keep files matching REGEXP. Taken from
https://protesilaos.com/emacs/dotemacs

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
    (interactive
     (list
      (read-regexp
       (concat "Files "
               (when current-prefix-arg
                 (propertize "NOT " 'face 'warning))
               "matching PATTERN: ")
       nil 'prot-dired--limit-hist)
      current-prefix-arg))
    (dired-mark-files-regexp regexp)
    (unless omit (dired-toggle-marks))
    (dired-do-kill-lines)
    (add-to-history 'prot-dired--limit-hist regexp))

  :bind
  (("C-x d" . dired-jump)
   ("C-M-d" . dired-jump)
   :map dired-mode-map
   ("M-o" . nil)
   ("C-o" . nil)
   ("q" . 'kill-current-buffer)
   ;; ("q" . 'bury-buffer)
   ("RET" . 'dired-find-alternate-file)
   ;; ("<M-return>" . 'dired-find-file)
   ("<M-return>" . 'dired-display-file)
   ("<S-return>" . 'acg/dired-display-file-and-next)
   ("<M-S-return>" . 'acg/dired-display-file-and-previous)
   ("<backspace>" . 'acg/dired-alternate-up-directory)
   ("<M-backspace>" . 'dired-up-directory)
   ([M-mouse-2] . 'dired-mouse-find-file-other-window) ;; not working @todo
   ([mouse-2] . 'acg/dired-mouse-find-alternate-file)))


(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-line-prefix "    ")
  :bind
  (:map dired-mode-map
   ("<tab>" . 'dired-subtree-cycle)
   ("<S-iso-lefttab>" . 'dired-subtree-remove)))


(use-package dired-sidebar
  :commands (dired-sidebar-show-sidebar)
  :config
  (defun acg/dired-sidebar-show-and-jump ()
    (interactive)
    (dired-sidebar-show-sidebar)
    (dired-sidebar-jump-to-sidebar))

  :bind
  (("C-d" . acg/dired-sidebar-show-and-jump)
   ("C-S-d" . dired-sidebar-toggle-with-current-directory)))


;; Dired ranger implements a "clipboard" for copying files in Dired.
;; https://github.com/Fuco1/dired-hacks#dired-ranger
(use-package dired-ranger
  :after dired)
