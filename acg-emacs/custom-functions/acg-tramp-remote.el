
(defun acg/with-default-directory-in-client-machine (fun)
  "Run function with `default-directory' on the client machine."
  (interactive)
  (let ((default-directory default-directory))
    (when (file-remote-p default-directory)
      (message "here")
      (setq default-directory "~/Downloads"))
    (funcall fun)))

(defun acg/get-local-or-remote-cd-command (filepath)
  "Returns a command that cds into a location, be it remote or
local."
  (if (file-remote-p filepath)
      (acg/get-remote-cd-command filepath)
    (format "cd %s" filepath)))

(defun acg/get-remote-cd-command (tramp-filepath)
  "Returns a command that cds into a remote location from a
TRAMP-type filename."
  (let* ((str-parts (split-string tramp-filepath ":"))
         (hostname (nth 1 str-parts))
         (dirpath (nth 2 str-parts)))
    (format "ssh -t %s 'cd %s && exec $SHELL'" hostname dirpath)))
