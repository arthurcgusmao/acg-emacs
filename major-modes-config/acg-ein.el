(require 'ein)

;; Changing the background color based on mode

;; (add-hook 'post-command-hook 'change-my-background-color)
;; (add-hook 'change-major-mode-hook 'change-my-background-color)
;; (add-hook 'window-configuration-change-hook 'change-my-background-color)

;; (defun change-my-background-color ()
;;   (cond
;;    ((eq major-mode 'org-mode)
;;     (set-background-color "honeydew"))
;;    ((eq major-mode 'text-mode)
;;     (set-background-color "blue"))
;;    (t
;;     (set-background-color "red"))))


;; (with-eval-after-load 'ein-notebooklist
;;   ;; removing keybindings
;;   (define-key ein:notebook-mode-map (kbd "M-p") nil)
;;   (define-key ein:notebook-mode-map (kbd "<M-up>") nil)
;;   (define-key ein:notebook-mode-map (kbd "<M-down>") nil)
;;   ;; changing keybinding
;;   (define-key ein:notebook-mode-map (kbd "C-s") 'ein:notebook-save-notebook-command)
;;   (define-key ein:notebook-mode-map (kbd "<M-S-up>") 'ein:worksheet-move-cell-up)
;;   (define-key ein:notebook-mode-map (kbd "<M-S-down>") 'ein:worksheet-move-cell-down)
;;   )

(use-package ein
  :defer t
  :commands ein:notebooklist-open
  :init
  (progn
    (with-eval-after-load 'ein-notebooklist
      ;; removing keybindings
      (define-key ein:notebook-mode-map (kbd "M-p") nil)
      (define-key ein:notebook-mode-map (kbd "<M-up>") nil)
      (define-key ein:notebook-mode-map (kbd "<M-down>") nil)
      (define-key ein:notebook-mode-map (kbd "<C-up>") nil)
      (define-key ein:notebook-mode-map (kbd "<C-down>") nil)
      
      ;; changing keybinding
      (define-key ein:notebook-mode-map (kbd "C-s") 'ein:notebook-save-notebook-command)
      (define-key ein:notebook-mode-map (kbd "<M-S-up>") 'ein:worksheet-move-cell-up)
      (define-key ein:notebook-mode-map (kbd "<M-S-down>") 'ein:worksheet-move-cell-down)
      (define-key ein:notebook-mode-map (kbd "<C-S-up>") 'ein:worksheet-goto-prev-input)
      (define-key ein:notebook-mode-map (kbd "<C-S-down>") 'ein:worksheet-goto-next-input)
      
      )))
