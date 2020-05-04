(use-package project
  :straight nil
  :bind (("M-o" . project-find-file)))

(require 'project)

(defcustom projectiny-known-projects-file
  (expand-file-name "projectiny-bookmarks"
                    user-emacs-directory)
  "File that references bookmarked projects.")

(defun projectiny-add-project ()
  "Add a project (with completion) to the known projects file.

The completion defaults to the root of the current project, which
uses `project-current' to provide a smart suggestion."
  (interactive)
  (let* ((default-dir (cdr (project-current nil)))
         (proj-dir
          (expand-file-name
           (read-directory-name "Choose the project directory: "
                                default-dir nil t)))
         (known-projects (projectiny--read-known-projects)))
    ;; Add new project to list
    (add-to-list 'known-projects proj-dir)
    ;; Write modified list to file
    (with-temp-buffer
      (insert (mapconcat 'identity known-projects "\n"))
      (write-file projectiny-known-projects-file))))

(defun projectiny-edit-known-projects ()
  "Open the `projectiny-known-projects-file' for editing.

Each line of the file should contain a directory path that
correspond to root directory of a project the user want to
bookmark."
  (interactive)
  (find-file projectiny-known-projects-file))

(defun projectiny--read-known-projects ()
  "Read the list of known projects from
`projectiny-known-projects-file'."
  (when (file-exists-p projectiny-known-projects-file)
    (with-temp-buffer
      (insert-file-contents projectiny-known-projects-file)
      (split-string (buffer-string) "\n" t))))

(defun projectiny--choose-project ()
  "Prompt the user to choose a project from the known list, and
return its root directory path. Shown paths are abbreviated to
increase readability."
  (expand-file-name
   (completing-read
    "Choose project: "
    (mapcar #'abbreviate-file-name
            (projectiny--read-known-projects)))))

(defun projectiny--project-get-instance (dir)
  "Return the project instance in DIR. If that directory is not a
part of a detectable project, return a `transient' project
instance rooted in it."
  (or (project--find-in-directory dir)
      (progn
        (message "Using `%s' as a transient project root" dir)
        (setq pr (cons 'transient dir)))))

(defun projectiny-find-file-in ()
  "Visit a file (with completion) in a project's roots.

The completion default is the filename at point, if one is
recognized."
  (interactive)
  (let* ((pr (projectiny--project-get-instance
              (projectiny--choose-project)))
        (dirs (project-roots pr)))
    (project-find-file-in (thing-at-point 'filename) dirs pr)))
;; @todo: Add additional keys to e.g. open magit of the project while
;; selecting the find (and then quit the minibuffer).

(defun projectiny-find-file-all ()
  "Visit a file (with completion) in all known projects.

The completion default is the filename at point, if one is
recognized."
  (interactive)
  (let* ((proj-instances
          (mapcar #'projectiny--project-get-instance
                  (projectiny--read-known-projects)))
         (all-files
          (mapcan #'project-files proj-instances)))
    (find-file (completing-read "Find file: " all-files))))


(global-set-key (kbd "C-c p") 'projectiny-find-file-in)
(global-set-key (kbd "M-O") 'projectiny-find-file-all)
