(require 'helm)

;; keybindings
(defun acg-helm-init ()
  (define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "<menu>") 'helm-M-x)
  (global-set-key (kbd "C-b") 'helm-mini)
  (global-set-key (kbd "C-o") 'helm-find-files)
  (global-set-key (kbd "C-S-B") 'helm-bookmarks)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  ;;(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; makes TAB work in terminal
  (define-key helm-map (kbd "M-a") 'helm-select-action) ; list actions using M-a
  (global-set-key (kbd "C-f") 'helm-swoop-without-pre-input)
  (global-set-key (kbd "C-S-F") 'helm-swoop)
  (global-set-key (kbd "C-M-f") 'helm-multi-swoop-all)
  (global-set-key (kbd "M-v") 'helm-show-kill-ring))


(add-hook 'after-init-hook 'acg-helm-init)

;; helm buffer always splits window and appears below
(setq helm-split-window-default-side 'below)
(setq helm-split-window-in-side-p t)
