(require 'crux)


(defun acg-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))


(defun acg-unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))



;; keybindings
(global-set-key (kbd "C-p") 'fill-paragraph)
(global-set-key (kbd "C-S-P") 'acg-unfill-paragraph)
(global-set-key (kbd "M-p") (crux-with-region-or-line fill-region))
(global-set-key (kbd "M-S-P") (crux-with-region-or-line acg-unfill-region))


;; settings -- max-width for lines when using fill commands
(setq-default fill-column 80)

(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 96)))
(add-hook 'tex-mode-hook
          (lambda ()
            (set-fill-column 96)))
(add-hook 'text-mode-hook
          (lambda ()
            (set-fill-column 96)))
