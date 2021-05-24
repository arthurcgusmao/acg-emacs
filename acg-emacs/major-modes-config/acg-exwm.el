;; (use-package exwm
;;   :config

;;   (defun exwm-config-ido ()
;;     "Configure Ido to work with EXWM."
;;     (ido-mode 1)
;;     (add-hook 'exwm-init-hook #'exwm-config--fix/ido-buffer-window-other-frame))

;;   (defun exwm-config-misc ()
;;     "Other configurations."
;;     ;; Make more room
;;     (menu-bar-mode -1)
;;     (tool-bar-mode -1)
;;     (scroll-bar-mode -1)
;;     (fringe-mode 1))


;;   ;; Set the initial workspace number.
;;   (unless (get 'exwm-workspace-number 'saved-value)
;;     (setq exwm-workspace-number 4))
;;   ;; Make class name the buffer name
;;   (add-hook 'exwm-update-class-hook
;;             (lambda ()
;;               (exwm-workspace-rename-buffer exwm-class-name)))
;;   ;; Global keybindings.
;;   (unless (get 'exwm-input-global-keys 'saved-value)
;;     (setq exwm-input-global-keys
;;           `(
;;             ;; 's-r': Reset (to line-mode).
;;             ([?\s-r] . exwm-reset)
;;             ;; 's-w': Switch workspace.
;;             ([?\s-w] . exwm-workspace-switch)
;;             ;; 's-&': Launch application.
;;             ([?\s-f1] . (lambda (command)
;;                          (interactive (list (read-shell-command "$ ")))
;;                          (start-process-shell-command command nil command)))
;;             ;; 's-N': Switch to certain workspace.
;;             ,@(mapcar (lambda (i)
;;                         `(,(kbd (format "s-%d" i)) .
;;                           (lambda ()
;;                             (interactive)
;;                             (exwm-workspace-switch-create ,i))))
;;                       (number-sequence 0 9)))))
;;   ;; Line-editing shortcuts
;;   (unless (get 'exwm-input-simulation-keys 'saved-value)
;;     (setq exwm-input-simulation-keys
;;           '(([?\C-b] . [left])
;;             ([?\C-f] . [right])
;;             ([?\C-p] . [up])
;;             ([?\C-n] . [down])
;;             ([?\C-a] . [home])
;;             ([?\C-e] . [end])
;;             ([?\M-v] . [prior])
;;             ([?\C-v] . [next])
;;             ([?\C-d] . [delete])
;;             ([?\C-k] . [S-end delete]))))
;;   ;; Enable EXWM
;;   (exwm-enable)
;;   ;; Configure Ido
;;   (exwm-config-ido)
;;   ;; Other configurations
;;   (exwm-config-misc))
