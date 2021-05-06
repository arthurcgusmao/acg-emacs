(use-package visual-fill-column
  :config
  (defun acg/toggle-visual-fill-column-and-center ()
    "Alternates between any of: (a) `visual-fill-column-mode'
disabled, (b) `visual-fill-column-mode' enabled, (c)
`visual-fill-column-mode' enabled with text centered."
    (interactive)
    (cond ((not visual-fill-column-mode)
           (visual-fill-column-mode 1))
          ((and visual-fill-column-mode
                (not visual-fill-column-center-text))
           (setq visual-fill-column-center-text t)
           (visual-fill-column-mode 1))
          (t
           (visual-fill-column-mode -1)
           (setq visual-fill-column-center-text nil))))
  :bind
  ("<S-f12>" . acg/toggle-visual-fill-column-and-center))

(use-package face-remap
  :commands acg/variable-pitch-mode
  :config
  (defun acg/update-fixed-pitch-font ()
    "Updates the fixed-pitch font using the font-family of the
default font.

Setting the fixed-pitch attributes should be done on the
fly since the value of the default font may have changed
(especially true when Emacs evaluates this code when a
frame is not created yet)."
    ;; Make fixed-pitch use the same attribute as the default face. For some
    ;; reason, using "Monospace" which is the default in faces.el created a
    ;; slightly different face.
    (let ((family (face-attribute 'default :family)))
      (set-face-attribute 'fixed-pitch nil
                          :family family)
      (set-face-attribute 'org-variable-pitch-fixed-face nil
                          :family family)))

  (define-minor-mode acg/variable-pitch-mode
    "Wrapper around `variable-pitch-mode'. Toggle
`variable-pitch-mode', except for `prog-mode'. If `org-mode',
toggle `org-variable-pitch-minor-mode' instead."
    :init-value nil
    :global nil
    (if acg/variable-pitch-mode
        (unless (derived-mode-p 'prog-mode)
          (acg/update-fixed-pitch-font)
          (if (eq major-mode 'org-mode)
              (org-variable-pitch-minor-mode 1)
            (variable-pitch-mode 1)))
      (variable-pitch-mode -1)
      (org-variable-pitch-minor-mode -1)))

  ;; List of possible fonts for variable pitch:
  ;; - Gentium Plus
  ;; - DejaVu Sans, Condensed
  (set-face-attribute 'variable-pitch nil
                      :family "DejaVu Sans"
                      :width 'condensed)

  :bind
  ("<f12>" . acg/variable-pitch-mode)
  :hook
  (org-mode . acg/variable-pitch-mode)
  (markdown-mode . acg/variable-pitch-mode))

(use-package org-variable-pitch
  :after face-remap)
