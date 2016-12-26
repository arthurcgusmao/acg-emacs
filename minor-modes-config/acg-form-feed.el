(require 'form-feed)
(require 'undo-tree)
(add-hook 'prog-mode-hook 'form-feed-mode)
;; (add-hook 'python-mode-hook 'form-feed-mode)
;; (add-hook 'emacs-lisp-mode-hook 'form-feed-mode)

;; (define-globalized-minor-mode acg-global-form-feed-mode form-feed-mode
;;   (lambda () (form-feed-mode 1)))

;; (acg-global-form-feed-mode 1)


;; keybindings

(defun insert-form-feed-below ()
  "Inserts form feed char below current line."
  (interactive)
  (if (acg-current-line-empty-p)
      (insert 12)
    (progn (end-of-line)
           (open-line 1)
           (next-line)
           (insert 12))))

(defun insert-form-feed-above ()
  "Inserts form feed char above current line."
  (interactive)
  (if (acg-current-line-empty-p)
      (insert 12)
    (progn (previous-line)
           (end-of-line)
           (open-line 1)
           (next-line)
           (insert 12))))

(define-key undo-tree-map (kbd "C-_") nil)
(global-set-key (kbd "C--") 'insert-form-feed-below)
(global-set-key (kbd "C-_") 'insert-form-feed-above)
