(require 'projectile)
(diminish 'projectile-mode)

;;(setq projectile-cache-file (expand-file-name  "projectile.cache" prelude-savefile-dir))
(projectile-global-mode t)

;; trying to fix projectile issue with tramp
(defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory) ad-do-it))

(require 'helm-projectile)
(helm-projectile-on)

;; ;; find string in projectile root dir in MS-Windows
;; ;; code below is working, but I decided to go for the solution of using git's grep
;; (defun acg-grep-projectile-windows(&optional search-string)
;;   (interactive)
;;   (let ((search-string (or search-string (read-string "[search in project] string pattern: "))))
;;     (shell-command-to-string (concat "findstr /s /i /p"
;;                            (concat "/c:\"" search-string "\" ")
;;                            (concat "\"" (replace-regexp-in-string "/" "\\" (projectile-project-root) t t) "*.*\"")))))

;; set keybindings
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-unset-key (kbd "M-o"))
(define-key projectile-mode-map (kbd "M-O") 'helm-projectile-switch-project)
(define-key projectile-mode-map (kbd "M-o") 'helm-projectile-find-file)
(global-unset-key (kbd "M-f"))
(define-key projectile-mode-map (kbd "M-f") 'helm-projectile-grep)

;; MS-Windows configs
(if (string-equal system-type "windows-nt")
    (progn
      ;; projectile's default grep was not working; setting it to use git grep
      ;; and having git installed on windows fixed.
      (setq projectile-use-git-grep t)
      ;; disable helm in grep because it wrecks on Windows
      (defun acg-projectile-grep-no-helm ()
        (interactive)
        (projectile-grep))
      ;; override keybindings
      (define-key projectile-mode-map (kbd "M-f") 'acg-projectile-grep-no-helm)))
