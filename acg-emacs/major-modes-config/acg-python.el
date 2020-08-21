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

;; Facilitate use of autopep8 inside Emacs.
;; https://pypi.org/project/autopep8/ (Install w/ `pip install autopep8')
(defun acg/autopep8-fill-paragraph ()
  (interactive)
  (acg/call-process-region
   "autopep8"
   `("--max-line-length" ,(number-to-string fill-column) "-a" "-")
   nil t 'mark-paragraph))
(defun acg/autopep8-buffer ()
  (interactive)
  (acg/call-process-region
   "autopep8" '("-") t nil 'mark-whole-buffer))

;; Facilitate use of black inside Emacs.
;; https://github.com/psf/black (Install w/ `pip install black')
(defun acg/py-black-fill-paragraph ()
  (interactive)
  (acg/call-process-region
   "xargs"
   `("-0" "black" "--line-length" ,(number-to-string fill-column) "--code")
   nil t 'mark-paragraph))
(defun acg/py-black-buffer ()
  (interactive)
  (acg/call-process-region
   "xargs"
   `("-0" "black" "--line-length" ,(number-to-string fill-column) "--code")
   t nil 'mark-whole-buffer))

;;;; DOCSTRINGS

;; Configure default behavior of fill-paragraph
(setq python-fill-docstring-style 'pep-257-nn)

;; Facilitate use of docformatter inside Emacs.
;; https://github.com/myint/docformatter (Install w/ `pip install docformatter')
(defun acg/py-docformatter-fill-paragraph ()
  (interactive)
  (acg/call-process-region
   "docformatter" '("-") nil t 'er/mark-outside-python-string))
(defun acg/py-docformatter-buffer ()
  (interactive)
  (acg/call-process-region
   "docformatter" '("-") t nil 'mark-whole-buffer))

;;;; Putting it all together

(defun acg/python-fill-paragraph ()
  "My version of python-fill-paragraph."
  (interactive)
  (cond ((python-info-docstring-p) (acg/py-docformatter-fill-paragraph))
        (t (acg/autopep8-fill-paragraph))))

(define-key python-mode-map (kbd "C-p") 'acg/python-fill-paragraph)


;; Fix newline indentation (indent according to previous blank line)
(add-hook
 'python-mode-hook
 (lambda () (setq acg/electric-indent-newline-as-previous-if-blank t)))
