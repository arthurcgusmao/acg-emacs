(use-package project
  :straight nil
  :bind (("M-o" . project-find-file)))

(require 'project)

(defcustom projectiny-known-projects-file
  (expand-file-name "projectiny-bookmarks"
                    user-emacs-directory)
  "File that references known projects.")

(defun projectiny-add-project ()
  "Add current project to known projects list."
  (interactive)
  (let* ((default-dir (cdr (project-current nil)))
         (proj-dir (read-directory-name
                    "Choose the project directory: "
                    default-dir nil t)))
    ;; (expand-file-name (cdr (project-current t)))
    (append-to-file
     (concat "\n" proj-dir)
     nil projectiny-known-projects-file)))

(defun projectiny-edit-known-projects ()
  "Open `projectiny-known-projects-file' for editing."
  (interactive)
  (find-file projectiny-known-projects-file))

(defun projectiny--read-known-projects ()
  "Read the list of known projects from
`projectiny-known-projects-file'."
  (with-temp-buffer
    (insert-file-contents projectiny-known-projects-file)
    (split-string (buffer-string) "\n" t)))

(defun projectiny--choose-project ()
  "Prompt the user to choose a project from the known list."
  (completing-read "Choose project: "
                   (projectiny--read-known-projects)))

(defun projectiny--project-get-instance (dir)
  "Return the project instance in DIR. If that directory is not a
part of a detectable project, return a `transient' project
instance rooted in it."
  (or (project--find-in-directory dir)
      (progn
        (message "Using `%s' as a transient project root" dir)
        (setq pr (cons 'transient dir)))))

(defun projectiny-find-file-in ()
  "Visit a file (with completion) in PR's roots.
The completion default is the filename at point, if one is
recognized."
  (interactive)
  (let* ((pr (projectiny--project-get-instance
              (projectiny--choose-project)))
        (dirs (project-roots pr)))
    (project-find-file-in (thing-at-point 'filename) dirs pr)))
;; @todo: Add additional keys to e.g. open magit of the project while
;; selecting the find (and then quit the minibuffer).

;; (defun projectiny--find-file-all ()
;;   "Visit a file (with completion) in all known projects.
;; The completion default is the filename at point, if one is
;; recognized."
;;   (let* ((dirs
;;           (mapcar (projectiny--read-known-projects) (project-roots pr))
;;           (mapcar #'message (projectiny--read-known-projects))
;;           (mapcar #'project-roots (projectiny--read-known-projects))
;;           ))
;;     (project-find-file-in (thing-at-point 'filename) dirs pr)))


(global-set-key (kbd "C-c p") 'projectiny-find-file-in)
