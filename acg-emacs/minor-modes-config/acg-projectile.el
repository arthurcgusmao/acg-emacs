(use-package projectile
  :config
  (setq projectile-use-git-grep t)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)
        ("M-o" . projectile-find-file))
  :hook
  (after-init . projectile-mode))

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
  :after (projectile)
  :hook (after-init . counsel-projectile-mode))
