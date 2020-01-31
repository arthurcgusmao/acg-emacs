(require 'multiple-cursors)

(global-set-key (kbd "C-c m m") 'mc/edit-lines)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m r") 'mc/mark-all-in-region)
(global-set-key (kbd "C-c m i l") 'mc/insert-letters)
(global-set-key (kbd "C-c m i n") 'mc/insert-numbers)
(global-set-key (kbd "C-c m v") 'yank)

(define-key mc/keymap (kbd "<escape>") 'mc/keyboard-quit)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-d") 'mc/mark-all-like-this)
(global-set-key (kbd "<M-S-up>") 'mc/mark-previous-lines)
(global-set-key (kbd "<M-S-down>") 'mc/mark-next-lines)

;; define custom location for file that saves which commands to run once/for-all
(setq mc/list-file (concat acg/acg-emacs-dir "others/.mc-lists.el"))
