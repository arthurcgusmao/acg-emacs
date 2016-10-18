(add-hook 'python-mode-hook 'anaconda-mode)

(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))
