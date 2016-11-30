(add-to-list 'load-path "~/.emacs.d/acg")
(add-to-list 'load-path "~/.emacs.d/acg/modules")
(add-to-list 'load-path "~/.emacs.d/acg/packages-config")
(add-to-list 'load-path "~/.emacs.d/acg/languages-config")

(setq acg-backup-dir "~/.backups/emacs/")


;; -------------------------------------------------------------------------
;; Adding Repositories
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(package-initialize)


;; -------------------------------------------------------------------------
;; Checking and installing defined packages

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(package-initialize)


;; -------------------------------------------------------------------------
;; Autoloading all files within directory

(defun my-load-all-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))


;; -------------------------------------------------------------------------
;; Other configurations

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; -------------------------------------------------------------------------
;; Requiring Files
(require 'acg-packages)
(require 'acg-editor)
(require 'acg-ui)
(require 'acg-keybindings)




(provide 'acg-core)
