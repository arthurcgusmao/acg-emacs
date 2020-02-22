(use-package projectile
  :config
  (projectile-global-mode t)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)
        ("M-o" . projectile-find-file)))

;;(setq projectile-cache-file (expand-file-name  "projectile.cache" prelude-savefile-dir))

;; trying to fix projectile issue with tramp
;; (defadvice projectile-project-root (around ignore-remote first activate)
;;   (unless (file-remote-p default-directory) ad-do-it))

;; ;; find string in projectile root dir in MS-Windows
;; ;; code below is working, but I decided to go for the solution of using git's grep
;; (defun acg/grep-projectile-windows(&optional search-string)
;;   (interactive)
;;   (let ((search-string (or search-string (read-string "[search in project] string pattern: "))))
;;     (shell-command-to-string (concat "findstr /s /i /p"
;;                            (concat "/c:\"" search-string "\" ")
;;                            (concat "\"" (replace-regexp-in-string "/" "\\" (projectile-project-root) t t) "*.*\"")))))


;; Integrations with completion packages

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode)
  :bind
  (:map projectile-mode-map
        ("M-f" . counsel-git-grep)))

;; (require 'helm-projectile)
;; (helm-projectile-on)

;; (define-key projectile-mode-map (kbd "M-O") 'helm-projectile-switch-project)
;; (define-key projectile-mode-map (kbd "M-o") 'helm-projectile-find-file)
;; (define-key projectile-mode-map (kbd "M-f") 'helm-projectile-grep)
;; (define-key helm-projectile-find-file-map (kbd "<C-backspace>") nil)


;; MS-Windows configs
(if (string-equal system-type "windows-nt")
    (progn
      ;; projectile's default grep was not working; setting it to use git grep
      ;; and having git installed on windows fixed.
      (setq projectile-use-git-grep t)
      ;; disable helm in grep because it wrecks on Windows
      (defun acg/projectile-grep-no-helm ()
        (interactive)
        (projectile-grep))
      ;; override keybindings
      (define-key projectile-mode-map (kbd "M-f") 'acg/projectile-grep-no-helm)))
