(add-hook 'python-mode-hook 'anaconda-mode)

(eval-after-load "company" 
  '(add-to-list 'company-backends 'company-anaconda))

;; (setenv "PYTHONPATH" "/home/arthurcgusmao/.anaconda2/bin/python")


;; customized indentation 

(defun acg-python-indent-line ()
  "Custom indentation for python major mode. Indents line to next
tab-stop if necessary."
  (interactive)
  (if (acg-current-line-empty-p)
      (python-indent-line nil)
    (let ((start-pos (point))
          (current-indentation (acg-current-indentation-column-p)))
      (let ((mod (% current-indentation python-indent-offset)))
        (if (> mod 0)
            (progn
              (indent-line-to (* (+ (/ current-indentation python-indent-offset)
                                    1) python-indent-offset))
              (goto-char (+ start-pos (- 4 mod)))))
        (if (< (current-column) (acg-current-indentation-column-p))
            (back-to-indentation))))))

(defun acg-python-override-indent-for-tab ()
  "Locally overrides the function `indent-for-tab-command'."
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'acg-python-indent-line))

(add-hook 'python-mode-hook 'acg-python-override-indent-for-tab)


;; keybindings
(add-hook 'python-mode-hook '(lambda ()
(define-key python-mode-map (kbd "<S-iso-lefttab>") 'python-indent-shift-left)
(define-key python-mode-map (kbd "C-<") 'python-indent-shift-left)
(define-key python-mode-map (kbd "C->") 'python-indent-shift-right)))


