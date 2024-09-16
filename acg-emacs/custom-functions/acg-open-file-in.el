;; some functions here were renamed to maintain consistency,
;; but they originally come from http://ergoemacs.org/emacs/

(defun acg/open-in-external-app (&optional -file-list)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-01-26"
  (interactive)
  (let* (
         (-file-list (or -file-list
                         (if (string-equal major-mode "dired-mode")
                             (dired-get-marked-files)
                           (list (buffer-file-name)))))
         (-do-it-p (if (<= (length -file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))

    (when -do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (fPath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) -file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  -file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) -file-list))))))


(defun acg/open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let (
          (process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ➢ for example: with nautilus
    )))


(defun acg/open-in-terminal ()
  "Open the current dir in a new terminal window.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-12-10"
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (cond
     ((string-equal system-type "windows-nt")
      ;; Custom AutoHotKey script file must be in path, see https://github.com/arthurcgusmao/acg-windows/blob/master/bin/cmder-in-dir.ahk
      (shell-command (concat "cmder-in-dir.ahk " dir)))
     ;; (w32-shell-execute "cmd" (replace-regexp-in-string "/" "\\" default-directory t t)))
     ((string-equal system-type "darwin")
      (shell-command (concat "open -a iTerm \"" dir "\"")))
     ((string-equal system-type "gnu/linux")
      (let ((process-connection-type nil))
        (start-process "" nil "x-terminal-emulator"
                       (concat "--working-directory=" dir) ))))))



;; keybindings
(global-set-key (kbd "C-c t") 'acg/open-in-terminal)
(global-set-key (kbd "C-c o") 'acg/open-in-desktop)
