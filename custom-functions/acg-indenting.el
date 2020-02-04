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

;; keybindings
(global-set-key (kbd "C-i") (crux-with-region-or-line indent-region))
(global-set-key (kbd "C-S-I") 'acg/indent-region-or-buffer)
