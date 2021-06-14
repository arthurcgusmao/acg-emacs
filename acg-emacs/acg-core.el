;; Defining directories variables

;; Change location of the automatically generated custom configurations
(setq custom-file (concat user-emacs-directory "custom.el"))

(defconst acg/acg-emacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where acg-emacs is installed.")

(defconst acg/default-bib-file "~/Documents/zotero.bib"
  "Default bibliography file for references.")

(defvar acg/file-backup-dir (concat user-emacs-directory "file-backups/")
  "Directory for backing up files.")
(defvar acg/scratch-backup-dir (concat user-emacs-directory "scratch-backups/")
  "Backup directory for scratch buffers.")
(defvar acg/history-dir (concat user-emacs-directory "history/")
  "Direcotry for saving various types of history in Emacs, such
  as minibuffer entries, recent opened files, cursor positions,
  etc.")

(if (not (file-exists-p acg/file-backup-dir))
    (make-directory acg/file-backup-dir t))
(if (not (file-exists-p acg/scratch-backup-dir))
    (make-directory acg/scratch-backup-dir t))
(if (not (file-exists-p acg/history-dir))
    (make-directory acg/history-dir t))


(defun acg/load-all-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

(defun append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(defun acg/add-to-env-path (path)
  "Add PATH (first position) to the PATH environment variable of
Emacs, used when running a shell in Emacs."
  (setenv "PATH" (concat (expand-file-name path)
                         path-separator (getenv "PATH"))))

(defun acg/add-to-env-path-and-exec-path (path)
  "Adds PATH to both the PATH env var and `exec-path'. The former
is used when running a shell; the latter is used for Emacs to
find executables."
  (acg/add-to-env-path path)
  (add-to-list 'exec-path (expand-file-name path)))

;; MacOS: add macports directory to executable path
(when (string-equal system-type "darwin")
  (acg/add-to-env-path-and-exec-path "~/.macports/bin")
  (acg/add-to-env-path-and-exec-path "~/.local/bin"))


;; Removing unwanted keybindings from local modes

(defvar acg/activated-major-modes-list
  '()
  "List that contains all modes and their respective mode-map.")

(defvar acg/global-keybindings-list
  '()
  "List that contains all keybindings that must be global.")

(defun acg/remove-all-local-keybindings ()
  "Removes keybindings in list `acg/glogal-keybindings-list' from
current local mode-map if mode is activated for the first time."
  (unless (member major-mode acg/activated-major-modes-list)
    (dolist (el acg/global-keybindings-list)
      (local-set-key (kbd el) nil))
    (add-to-list 'acg/activated-major-modes-list major-mode)))

(add-hook 'after-change-major-mode-hook 'acg/remove-all-local-keybindings)

(defun acg/force-global-set-key (keystring keyfunc)
  "Globally assigns to the keybinding (1st argument) the
function (2nd argument); also removes from all mode-maps local
bindings to the same command."
  (add-to-list 'acg/global-keybindings-list keystring)
  (global-set-key (kbd keystring) keyfunc))


;; PACKAGE MANAGER

;; Integrate `use-package' with `straight.el'
(straight-use-package 'use-package)

;; Make use-package install packages using `straight.el' by default.
(setq straight-use-package-by-default t)
;; Note: to go back to the default behavior (e.g., to configure a built-in
;; package you don't want to build from source), set the keyword `:straight' to
;; `nil' in the use-package declaration.


;; Requiring Files

(load "acg-editor")
(acg/load-all-in-directory (concat acg/acg-emacs-dir "custom-functions"))
(acg/load-all-in-directory (concat acg/acg-emacs-dir "minor-modes-config"))
(acg/load-all-in-directory (concat acg/acg-emacs-dir "major-modes-config"))
(load "acg-ui")


(provide 'acg-core)
