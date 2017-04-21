;; Defining directories variables
(defconst default-emacs-dir "~/.emacs.d/"
  "Directory where init.el is located (usually ~/.emacs.d/)")

(defconst acg-emacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where acg-emacs is installed.")

(setq acg-backup-dir "~/.backups/emacs/")


;; Adding Repositories
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(package-initialize)


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


;; Removing unwanted keybindings from local modes

(defvar acg-activated-major-modes-list
  '()
  "List that contains all modes and their respective mode-map.")

(defvar acg-global-keybindings-list
  '()
  "List that contains all keybindings that must be global.")

(defun acg-remove-all-local-keybindings ()
  "Removes keybindings in list `acg-glogal-keybindings-list' from
current local mode-map if mode is activated for the first time."
  (unless (member major-mode 'acg-activated-major-modes-list)
    (dolist (el acg-global-keybindings-list)
      (local-set-key (kbd el) nil))
    (add-to-list 'acg-activated-major-modes-list major-mode)))

(add-hook 'after-change-major-mode-hook 'acg-remove-all-local-keybindings)

(defun acg-force-global-set-key (keystring keyfunc)
  "Globally assigns to the keybinding (1st argument) the
function (2nd argument); also removes from all mode-maps local
bindings to the same command."
  (add-to-list 'acg-global-keybindings-list keystring)
  (global-set-key (kbd keystring) keyfunc))


;; Requiring Files
(require 'acg-keybindings)
(require 'acg-packages)
(require 'acg-editor)
(require 'acg-ui)


;; Other configurations

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; make EMACS use the PATHs specified in .bashrc
(require 'exec-path-from-shell)
(when (memq window-system '(x))
  (exec-path-from-shell-initialize))


(provide 'acg-core)
