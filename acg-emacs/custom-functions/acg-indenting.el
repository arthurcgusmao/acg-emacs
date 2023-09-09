(defun acg/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun acg/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (acg/indent-buffer)
        (message "Indented buffer.")))))

;; ;; keybindings
;; (global-set-key (kbd "M-i") (crux-with-region-or-line indent-region))
;; (global-set-key (kbd "M-I") 'acg/indent-region-or-buffer)


;; Fix newline indentation in electric-mode, for modes like Python

(defvar-local acg/electric-indent-newline-as-previous-if-blank nil
  "Buffer-local variable that indicates one wants to have
`electric-indent-post-self-insert-function' indent a newly
inserted line with the same indentation as the previous line, if
the previous line was a blank line. This variable is used in
`acg/advice--electric-indent-post-self-insert-function'.

Particularly, I find this behavior quite useful in Python, as
discussed in https://emacs.stackexchange.com/q/53153/13589")

(defun acg/advice--electric-indent-post-self-insert-function (orig-fun)
  "Advice to be put around `electric-indent-post-self-insert-function';
see `acg/electric-indent-newline-as-previous-if-blank'."
  (let (pos prev-indent prev-line-blank-p)
    (if (and acg/electric-indent-newline-as-previous-if-blank
             (save-excursion
               (previous-line)
               (setq prev-line-blank-p (acg/line-empty-p))))
        ;; Section below is part of the original function that I adapted
        (when (and electric-indent-mode
                   ;; Don't reindent while inserting spaces at beginning of line.
                   (or (not (memq last-command-event '(?\s ?\t)))
                       (save-excursion (skip-chars-backward " \t") (not (bolp))))
                   (setq pos (electric--after-char-pos))
                   (save-excursion
                     (goto-char pos)
                     (let ((act (or (run-hook-with-args-until-success
                                     'electric-indent-functions
                                     last-command-event)
                                    (memq last-command-event electric-indent-chars))))
                       (not
                        (or (memq act '(nil no-indent))
                            ;; In a string or comment.
                            (unless (eq act 'do-indent) (nth 8 (syntax-ppss))))))))
          ;; Get value of previous indentation
          (save-excursion
            (previous-line)
            (setq prev-indent (current-indentation)))
          ;; Indent current line catching errors
          (let ((at-newline (<= pos (line-beginning-position))))
            (unless (and electric-indent-inhibit
                         (not at-newline))
              (condition-case-unless-debug ()
                  ;; (indent-according-to-mode)
                  (indent-line-to prev-indent)
                (error (throw 'indent-error nil))))))
      ;; If not using modification or not blank line above, back to default
      (funcall orig-fun))))

(advice-add 'electric-indent-post-self-insert-function :around #'acg/advice--electric-indent-post-self-insert-function)
;; (advice-remove 'electric-indent-post-self-insert-function #'acg/advice--electric-indent-post-self-insert-function)
