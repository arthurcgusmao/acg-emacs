(use-package tab-line
  :straight nil
  :config
  ;;; Make tabs show modified state

  (defun acg/tab-line-tab-name-buffer (buffer &optional _buffers)
    "Custom version of `tab-line-tab-name-buffer', function that
generates the name of each tab in tab-line."
    (let* ((buffer-name (buffer-name buffer))
           (tab-name (concat " " buffer-name " ")))
      (if (and (buffer-modified-p buffer)
               (or (buffer-file-name buffer)
                   (acg/scratch-buffer-p buffer)))
          (concat " •" tab-name)
        tab-name)))

  (defun acg/update-tab-line-format-all-tabs ()
    "Same as `update-tab-line-format' but disregards cache and
for all tabs. To be used when one wants to 'forcefully' update
all tabs state."
    (dolist (window (window-list-1 nil 'ignore 'visible))
      (let* ((tabs (funcall tab-line-tabs-function))
             (cache-key (list tabs
                              (window-buffer)
                              (window-parameter window 'tab-line-hscroll))))
        (cdr (set-window-parameter
              window 'tab-line-cache
              (cons cache-key (tab-line-format-template tabs)))))))

  (defun acg/update-tab-line-format-first-change ()
    (set-buffer-modified-p t)
    (acg/update-tab-line-format-all-tabs))

  (setq tab-line-tab-name-function 'acg/tab-line-tab-name-buffer)
  (setq tab-line-close-button-show nil) ; Never show "x" (close tab) button

  ;; Handle updates of the buffer modified indicator
  (add-hook 'after-save-hook 'acg/update-tab-line-format-all-tabs)
  (add-hook 'first-change-hook 'acg/update-tab-line-format-first-change 90)

  ;; Update when buffer unmodified -- requires `unmodified-buffer' to be loaded first
  (eval-after-load 'unmodified-buffer
    (add-hook 'unmodified-buffer-hook 'acg/update-tab-line-format-all-tabs))


  ;;; Order buffers by first opened

  ;; Dissociate tab-line from window buffers. What I want to have is tab-line
  ;; as a completely separate abstraction, where the tabs order is a separate
  ;; ordering.
  (defun acg/tab-line-tabs-func ()
    "Wrapper around `tab-line-tabs-window-buffers' that preserves
the original order of the buffers in the tab-line."
    (let* ((order (window-parameter nil 'tab-line-order)) ; Use window-parameter to store order
           (order (seq-filter #'buffer-live-p order))     ; Filter out dead buffers
           (order (seq-filter #'acg/tab-line-filter order))) ; Filter desired buffers
      ;; Add current buffer to list, if missing
      (unless (member (current-buffer) order)
        (setq order (append order (list (current-buffer)))))
      (set-window-parameter nil 'tab-line-order order)))

  (setq tab-line-tabs-function 'acg/tab-line-tabs-func)
  ;; (setq tab-line-tabs-function 'tab-line-tabs-window-buffers)
  ;; (set-window-parameter nil 'tab-line-order nil)

  ;; Filter only desired buffers
  (defun acg/tab-line-filter (buffer)
    "Filter only buffers that should be displayed in tab-line."
    (let ((buffer-name (string-trim-left (buffer-name buffer))))
      (if (eq buffer (current-buffer)) t  ; Never discard current-buffer
        (not (or
              (string-equal "*" (substring buffer-name 0 1)) ; Remove * Emacs buffers
              )))))


  ;;; Move better across tabs

  (defun acg/tab-line-get-current-pos ()
    "Return current position in the tab-line."
    (let ((tabs (funcall tab-line-tabs-function)))
      (seq-position
       tabs (current-buffer)
       (lambda (tab buffer)
         (if (bufferp tab)
             (eq buffer tab)
           (eq buffer (cdr (assq 'buffer tab))))))))

  (defun acg/tab-line-goto-pos (pos)
    "Switches to the tab number POS in the selected window.
Exhibits recursive or loop-like behavior when POS is < 0 or
> (length tabs)."
    (interactive)
    (let* ((tabs (funcall tab-line-tabs-function))
           (pos (mod pos (length tabs))) ; Correction to have loop-like behavior
           (tab (nth pos tabs))
           (buffer (if (bufferp tab) tab (cdr (assq 'buffer tab)))))
      (switch-to-buffer buffer)))

  (defun acg/tab-line-walk (delta)
    "Moves forward DELTA tabs in the selected window."
    (acg/tab-line-goto-pos (+ (acg/tab-line-get-current-pos) delta)))

  ;; Redefine existing functions
  (defun tab-line-switch-to-next-tab (&optional mouse-event)
    "@acg modification for `tab-line-switch-to-next-tab'."
    (interactive (list last-nonmenu-event))
    (let ((window (and (listp mouse-event) (posn-window (event-start mouse-event)))))
      (with-selected-window (or window (selected-window))
        (acg/tab-line-walk 1))))
  (defun tab-line-switch-to-prev-tab (&optional mouse-event)
    "@acg modification for `tab-line-switch-to-prev-tab'."
    (interactive (list last-nonmenu-event))
    (let ((window (and (listp mouse-event) (posn-window (event-start mouse-event)))))
      (with-selected-window (or window (selected-window))
        (acg/tab-line-walk -1))))

  ;; TODO: modify <M-q> to operate only on tabs when tab-line is active

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
