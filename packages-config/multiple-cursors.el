(require 'multiple-cursors)

(global-set-key (kbd "C-c m e") 'mc/edit-lines)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m r") 'mc/mark-all-in-region)
(global-set-key (kbd "C-c m i l") 'mc/insert-letters)
(global-set-key (kbd "C-c m i n") 'mc/insert-numbers)
