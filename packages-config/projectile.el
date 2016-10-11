(require 'projectile)
;;(setq projectile-cache-file (expand-file-name  "projectile.cache" prelude-savefile-dir))
(projectile-global-mode t)

(require 'helm-projectile)
(helm-projectile-on)
