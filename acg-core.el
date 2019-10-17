;; Defining directories variables
(defconst default-emacs-dir "~/.emacs.d/"
  "Directory where init.el is located (usually ~/.emacs.d/)")

;; Change location of the automatically generated custom configurations
(setq custom-file (concat default-emacs-dir "custom.el"))

(defconst acg-emacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where acg-emacs is installed.")

;; Add miniconda binaries to executables path
(if (string-equal system-type "gnu/linux")
    (add-to-list 'exec-path "~/.miniconda3/bin"))

(defconst acg-default-bib-file "~/Documents/Mendeley/library.bib"
  "Default bibliography file for references.")

(defvar acg-backup-dir (concat user-emacs-directory "backups/")
  "Emacs's default directory for backing up files.")
(defvar acg-scratch-backup-dir (concat user-emacs-directory "scratch-backups/")
  "Backup directory for scratch buffers.")

(if (not (file-exists-p acg-backup-dir))
        (make-directory acg-backup-dir t))
(if (not (file-exists-p acg-scratch-backup-dir))
        (make-directory acg-scratch-backup-dir t))


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
  (unless (member major-mode acg-activated-major-modes-list)
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

;; make EMACS use the PATHs specified in .bashrc
(require 'exec-path-from-shell)
(when (memq window-system '(x))
  (exec-path-from-shell-initialize))


(provide 'acg-core)
