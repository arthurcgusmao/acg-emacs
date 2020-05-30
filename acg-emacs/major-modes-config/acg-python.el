(use-package anaconda-mode)
(use-package company-anaconda)

(add-hook 'python-mode-hook 'anaconda-mode)

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))


;; tab-width
(add-hook 'python-mode-hook
      (lambda ()
        ;; (setq indent-tabs-mode t)
        (setq tab-width 4)
        (setq python-indent 4)))


;; customized indentation

(defun acg/python-indent-line ()
  "Custom indentation for python major mode. Indents line to next
tab-stop if necessary."
  (interactive)
  (if (acg/line-empty-p)
      (python-indent-line nil)
    (let ((start-pos (point))
          (current-indentation (acg/current-indentation-column-p)))
      (let ((mod (% current-indentation python-indent-offset)))
        (if (> mod 0)
            (progn
              (indent-line-to (* (+ (/ current-indentation python-indent-offset)
                                    1) python-indent-offset))
              (goto-char (+ start-pos (- 4 mod)))))
        (if (< (current-column) (acg/current-indentation-column-p))
            (back-to-indentation))))))

(defun acg/python-override-indent-for-tab ()
  "Locally overrides the function `indent-for-tab-command'."
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'acg/python-indent-line))

(add-hook 'python-mode-hook 'acg/python-override-indent-for-tab)
;; (remove-hook 'python-mode-hook 'acg/python-override-indent-for-tab)

(defun acg/python-shell-send-region (beg end)
  "Similar to `python-shell-send-region' but displays output if
any, similar to what a Jupyter REPL would do."
  (interactive "r")
  (python-shell-send-string
   (buffer-substring beg end)))

;; keybindings
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "<S-iso-lefttab>") 'python-indent-shift-left)
             (define-key python-mode-map [control-bracketleft] 'python-indent-shift-left)
             (define-key python-mode-map (kbd "C-]") 'python-indent-shift-right)
             (define-key python-mode-map (kbd "<f7>") 'python-shell-switch-to-shell)
             (define-key python-mode-map (kbd "<f8>") 'python-shell-send-buffer)
             (define-key python-mode-map (kbd "C-c C-y") 'run-python)
             ; The functions below are just sending the code to the shell
             ; ("invisibly"), but not printing it. @TODO: create a new function
             ; that prints the results of the last variable, so that it works
             ; like the Jupyter REPL.
             (define-key python-mode-map (kbd "C-c C-b") (acg/eval-with 'acg/python-shell-send-region 'mark-whole-buffer))
             (define-key python-mode-map (kbd "C-c C-p") (acg/eval-with 'acg/python-shell-send-region 'mark-page))
             (define-key python-mode-map (kbd "C-c C-c") (acg/eval-with 'acg/python-shell-send-region 'acg/mark-dwim))
             (define-key python-mode-map (kbd "C-c C-l") (acg/eval-with 'python-shell-send-string 'acg/expand-region-to-whole-lines 'acg/unindent-add-last-var))))

(use-package python-docstring
  :bind (:map python-docstring-mode-map
         ("C-p" . python-docstring-fill)
         ("M-q" . nil)))


;; Fix newline indentation in electric-mode

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

          ;; (catch 'indent-error
          ;;   (unless electric-indent-inhibit
          ;;     (condition-case-unless-debug ()
          ;;         (indent-line-to prev-indent)
          ;;       (error (throw 'indent-error nil)))))

          (let ((at-newline (<= pos (line-beginning-position))))
            ;; (when at-newline
            ;;   (let ((before (copy-marker (1- pos) t)))
            ;;     (save-excursion
            ;;       (unless
            ;;           (or (memq indent-line-function
            ;;                     electric-indent-functions-without-reindent)
            ;;               electric-indent-inhibit)
            ;;         ;; Don't reindent the previous line if the
            ;;         ;; indentation function is not a real one.
            ;;         (goto-char before)
            ;;         (condition-case-unless-debug ()
            ;;             ;; (indent-according-to-mode)
            ;;             (indent-line-to prev-indent)
            ;;           (error (throw 'indent-error nil))))
            ;;       (unless (eq electric-indent-inhibit 'electric-layout-mode)
            ;;         ;; Unless we're operating under
            ;;         ;; `electric-layout-mode' (Bug#35254), the goal here
            ;;         ;; will be to remove the trailing whitespace after
            ;;         ;; reindentation of the previous line because that
            ;;         ;; may have (re)introduced it.
            ;;         (goto-char before)
            ;;         ;; We were at EOL in marker `before' before the call
            ;;         ;; to `indent-according-to-mode' but after we may
            ;;         ;; not be (Bug#15767).
            ;;         ;; (when (and (eolp))
            ;;         ;;   (delete-horizontal-space t))
            ;;         ))))
            (unless (and electric-indent-inhibit
                         (not at-newline))
              (condition-case-unless-debug ()
                  ;; (indent-according-to-mode)
                  (indent-line-to prev-indent)
                (error (throw 'indent-error nil)))))

          )
      ;; If not using modification or not blank line above, back to default
      (funcall orig-fun))))

(advice-add 'electric-indent-post-self-insert-function :around #'acg/advice--electric-indent-post-self-insert-function)
;; (advice-remove 'electric-indent-post-self-insert-function #'acg/advice--electric-indent-post-self-insert-function)

(add-hook
 'python-mode-hook
 (lambda () (setq acg/electric-indent-newline-as-previous-if-blank t)))
