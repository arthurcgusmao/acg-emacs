
(defcustom etlassian-urls-file
  (expand-file-name "etlassian-urls"
                    user-emacs-directory)
  "File that references known projects.")


(defun etlassian--read-urls ()
  "Read the list of known projects from
`etlassian-urls-file'."
  (when (file-exists-p etlassian-urls-file)
    (with-temp-buffer
      (insert-file-contents etlassian-urls-file)
      (split-string (buffer-string) "\n" t))))

(defun etlassian--choose-url ()
  "Prompt the user to choose an URL from the known list, and
return it."
  (let ((urls (etlassian--read-urls)))
    (if (= (length urls) 1)
        (car urls)
      (completing-read "Choose atlassian URL: " urls))))

(defun etlassian-edit-urls ()
  "Open `etlassian-urls-file' for editing.

Each line of the file should contain a URL path that correspond
to the URL of an atlassian base URL the user wants etlassian to
remember."
  (interactive)
  (find-file etlassian-urls-file))

(defun etlassian-open-jira-issue (&optional issue-id)
  "Opens a Jira issue based on smart assumptions.

Initial assumption is the Git branch of the current project."
  (interactive)
  (let* ((url (file-name-as-directory (etlassian--choose-url)))
         issue-id)
    ;; Define issue ID
    (setq issue-id (car (split-string (magit-get-current-branch)  "_")))
    ;; Open URL
    (setq url (concat url "browse/" issue-id))
    (browse-url url)
    (message "Opening %s" url)))


(provide 'etlassian)

;;; etlassian.el ends here



;; My customizations

(setq etlassian-urls-file
      (concat acg/history-dir "etlassian-urls"))
