;; Writable Grep package
;;   Use in rg.el (ripgrep): key `e' enables editing
;;   Use after ivy-occur (C-c C-o): key `w' enables editing
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :bind
  (:map wgrep-mode-map
        ("C-s" . wgrep-finish-edit)
        ("C-w" . wgrep-abort-changes)
        :map grep-mode-map
        ("C-e" . wgrep-change-to-wgrep-mode)))

;; Riggrep package
;;  Configs adapted from https://protesilaos.com/dotemacs/
(use-package rg
  :ensure t
  :after wgrep
  :config
  (setq rg-group-result t)
  (setq rg-hide-command t)
  (setq rg-show-columns nil)
  (setq rg-show-header t)
  (setq rg-custom-type-aliases nil)
  (setq rg-default-alias-fallback "all")

  (rg-define-search prot/grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                         ; search root project dir
             default-directory))          ; or from the current dir
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (defun prot/rg-save-search-as-name ()
    "Save `rg' buffer, naming it after the current search query.

This function is meant to be mapped to a key in `rg-mode-map'."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "«" pattern "»"))))

  :bind (("M-s g" . prot/grep-vc-or-dir)
         :map rg-mode-map
         ("s" . prot/rg-save-search-as-name)
         ;; ("C-n" . next-line)
         ;; ("C-p" . previous-line)
         ;; ("M-n" . rg-next-file)
         ;; ("M-p" . rg-prev-file)
         ))
