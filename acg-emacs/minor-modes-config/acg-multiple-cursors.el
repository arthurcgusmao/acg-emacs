(use-package multiple-cursors
  :config
  ;; define custom location for file that saves which commands to run once/for-all
  (setq mc/list-file (concat acg/acg-emacs-dir "others/.mc-lists.el"))
  :bind
  (("C-c m m" . mc/edit-lines)
   ("C-c m n" . mc/mark-next-like-this)
   ("C-c m p" . mc/mark-previous-like-this)
   ("C-c m a" . mc/mark-all-like-this)
   ("C-c m s" . mc/mark-all-symbols-like-this)
   ("C-c m r" . mc/mark-all-in-region)
   ("C-c m i l" . mc/insert-letters)
   ("C-c m i n" . mc/insert-numbers)
   ("C-c m v" . yank)
   ("C-c m u p" . mc/mark-previous-like-this)
   ("C-c m u n" . mc/mark-next-like-this)

   ("M-d" . mc/mark-next-like-this)
   ("M-D" . mc/mark-all-like-this)
   ("<C-S-up>" . mc/mark-previous-lines)
   ("<C-S-down>" . mc/mark-next-lines)

   ("<M-down-mouse-1>" . mc/add-cursor-on-click)
   :map mc/keymap
   ("<escape>" . mc/keyboard-quit)))
