(use-package tab-line
  :straight nil
  :config
  ;; (global-tab-line-mode t)

  (defun acg/tab-line-tab-name-buffer (buffer &optional _buffers)
    "Custom version of `tab-line-tab-name-buffer', function that
generates the name of each tab in tab-line."
    (let ((buffer-name (buffer-name buffer)))
      (if (and (buffer-modified-p buffer)
               (or (buffer-file-name buffer)
                   (acg/scratch-buffer-p buffer)))
          (concat "✎ " buffer-name)
        buffer-name)))

  (defun acg/update-tab-line-format ()
    "Same as `update-tab-line-format' but disregards cache."
    (let* ((tabs (funcall tab-line-tabs-function))
           (cache-key (list tabs
                            (window-buffer)
                            (window-parameter nil 'tab-line-hscroll))))
      (cdr (set-window-parameter
            nil 'tab-line-cache
            (cons cache-key (tab-line-format-template tabs))))))

  (defun acg/update-tab-line-format-first-change ()
    (set-buffer-modified-p t)
    (acg/update-tab-line-format))

  (setq tab-line-tab-name-function 'acg/tab-line-tab-name-buffer)
  (setq tab-line-close-button-show nil) ; Never show "x" (close tab) button

  ;; Handle updates of the buffer modified indicator
  (add-hook 'after-save-hook 'acg/update-tab-line-format)
  (add-hook 'first-change-hook 'acg/update-tab-line-format-first-change)

  ;; Update when buffer unmodified -- requires `unmodified-buffer' to be loaded first
  (eval-after-load 'unmodified-buffer
    (add-to-list 'unmodified-buffer-hook 'acg/update-tab-line-format t))

  :bind
  (("<C-tab>" . tab-line-switch-to-next-tab)
   ("<C-S-iso-lefttab>" . tab-line-switch-to-prev-tab) ;; for Linux
   ("<C-S-tab>" . tab-line-switch-to-prev-tab) ;; for Windows
   ("<C-next>" . tab-line-switch-to-next-tab)
   ("<C-prior>" . tab-line-switch-to-prev-tab)
   ;; Key combos "C-S-PgUp" and "C-S-PgDn" move the current tab to the left and to the right.
   ("C-S-<prior>" . tabbar-move-current-tab-one-place-left)
   ("C-S-<next>" . tabbar-move-current-tab-one-place-right))
  :hook
  ((after-init . global-tab-line-mode)))
