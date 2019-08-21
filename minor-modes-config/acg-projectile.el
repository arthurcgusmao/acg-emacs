(require 'projectile)
(diminish 'projectile-mode)

;;(setq projectile-cache-file (expand-file-name  "projectile.cache" prelude-savefile-dir))
(projectile-global-mode t)

;; trying to fix projectile issue with tramp
(defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory) ad-do-it))

(require 'helm-projectile)
(helm-projectile-on)

;; set keybindings
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
