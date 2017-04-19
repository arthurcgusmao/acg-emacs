(require 'helm)

;; keybindings
(defun acg-helm-init ()
  (define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)

  (define-key helm-map (kbd "<C-backspace>") nil)
  (define-key helm-read-file-map (kbd "<C-backspace>") nil)
  (define-key helm-find-files-map (kbd "<C-backspace>") nil)

  (define-key helm-map (kbd "<left>") nil)
  (define-key helm-read-file-map (kbd "<left>") nil)
  (define-key helm-find-files-map (kbd "<left>") nil)

  (define-key helm-map (kbd "<right>") nil)
  (define-key helm-read-file-map (kbd "<right>") nil)
  (define-key helm-find-files-map (kbd "<right>") nil)
  
  ;; (define-key helm-find-files-map (kbd "<backtab>") 'helm-find-files-up-one-level)
  
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
  (global-set-key (kbd "M-v") 'helm-show-kill-ring)

  ;; see HELM FIND FILES configs section below
  (define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)
  )


(add-hook 'after-init-hook 'acg-helm-init)

;; helm buffer always splits window and appears below
(setq helm-split-window-default-side 'below)
(setq helm-split-window-in-side-p t)


;; HELM FIND FILES configs
;; from https://emacs.stackexchange.com/questions/3798/how-do-i-make-pressing-ret-in-helm-find-files-open-the-directo$

;; makes RET in helm-find-files open directory in itself instead of in dired mode
(defun acg-helm-find-files-navigate-forward (orig-fun &rest args)
  (if (and (equal "Find Files" (assoc-default 'name (helm-get-current-source)))
           (equal args nil)
           (stringp (helm-get-selection))
           (not (file-directory-p (helm-get-selection))))
      (helm-maybe-exit-minibuffer)
    (apply orig-fun args)))
(advice-add 'helm-execute-persistent-action :around #'acg-helm-find-files-navigate-forward)
;; return key defined at keybindings section above

;; makes backspace go back 1 directory level when on the beginning of the directory
(defun acg-helm-find-files-navigate-back (orig-fun &rest args)
  (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
      (helm-find-files-up-one-level 1)
    (apply orig-fun args)))
(advice-add 'helm-ff-delete-char-backward :around #'acg-helm-find-files-navigate-back)
