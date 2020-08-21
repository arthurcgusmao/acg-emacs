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


;;; CODE FORMATTERS and related

;;;; CODE ONLY (not docstrings)

;; Facilitates use of autopep8 inside Emacs.
;; https://pypi.org/project/autopep8/ (Install w/ `pip install autopep8')
;; https://github.com/paetzke/py-autopep8.el
(use-package py-autopep8
  :commands (py-autopep8-buffer))

;;;; DOCSTRINGS

;; Configure default behavior of fill-paragraph
(setq python-fill-docstring-style 'pep-257-nn)

;; Facilitates use of docformatter inside Emacs.
;; https://github.com/myint/docformatter (Install w/ `pip install docformatter')
;; https://github.com/humitos/py-docformatter.el
(use-package py-docformatter
  :straight (:host github :repo "humitos/py-docformatter.el")
  :config
  (setq py-docformatter-options nil)
  :commands (py-docformatter-buffer))

(defun acg/py-docformatter-dwim ()
  "Replaces function at point with the output of docformatter.
Docformatter handles only docstrings. If region is active, run
docformatter on that."
  (interactive)
  (progn
    (let ((c (current-column))
          (l (line-number-at-pos))
          (old-buf-size (buffer-size)))
      ;; (p (point))
      (unless (region-active-p)
        (if 'er/mark-outside-python-string
            (progn (er/mark-outside-python-string)
                   (beginning-of-line))
          (mark-defun)))
      (let ((beg (region-beginning))
            (end (region-end)))
        (call-process-region beg end "docformatter" t t nil
                             (concat py-docformatter-options "-"))
        ;; Refontify altered region
        (let ((new-end (+ end (- (buffer-size) old-buf-size))))
          (font-lock-fontify-region beg new-end nil))
        ;; Restore position in buffer outside `save-excursion' because we want to
        ;; get as close as we originally were. Unfortunately `save-excursion'
        ;; does not behave as expected here.
        (goto-line l)
        (move-to-column c)))))

;;;; Putting it all together

(defun acg/python-fill-paragraph ()
  "My version of python-fill-paragraph."
  (interactive)
  (cond ((python-info-docstring-p) (acg/py-docformatter-dwim))
        (t (python-fill-paragraph))))

(define-key python-mode-map (kbd "C-p") 'acg/python-fill-paragraph)



;; Fix newline indentation (indent according to previous blank line)
(add-hook
 'python-mode-hook
 (lambda () (setq acg/electric-indent-newline-as-previous-if-blank t)))
