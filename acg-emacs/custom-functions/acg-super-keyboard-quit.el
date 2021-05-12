(defun acg/super-keyboard-quit ()
  "Run many similar quit commands to make this function a general quit."
  (interactive)
  (keyboard-quit)
  (abort-recursive-edit)
  (setq quit-flag t))

(defun acg/dummy-quit ()
  "Function that mimics quit to use when canceling a keyboard
sequence."
  (interactive)
  (message "Quit"))


;;; Keybindings

;; ESC always quits
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(eval-after-load "anaconda-mode-view-mode" '(define-key anaconda-mode-view-mode-map [escape] 'kill-buffer-and-window))
(global-set-key [escape] 'acg/super-keyboard-quit)
(acg/force-global-set-key "C-c <escape>" 'acg/dummy-quit)
;; rebind ESC functions to C-<escape>
(define-key key-translation-map (kbd "C-<escape>") (kbd "ESC"))

(use-package help-mode
  :straight nil
  :bind
  (:map help-mode-map
        ("<escape>" . kill-buffer-and-window)))
