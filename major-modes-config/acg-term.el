(require 'multi-term) ;; See description of the package in https://www.emacswiki.org/emacs/MultiTerm

(setq multi-term-program "/bin/bash")

(defun acg/new-term ()
  (interactive)
  (multi-term))


;; customizing colors
(require 'eterm-256color)

;; 256 color support
(add-hook 'term-mode-hook #'eterm-256color-mode)
;; (remove-hook 'term-mode-hook #'eterm-256color-mode)

;; allow bold faces
(setq eterm-256color-disable-bold nil)

;; (custom-set-variables
;;  '(term-default-bg-color "#000000")        ;; background color (black)
;;  '(term-default-fg-color "#dddd00"))       ;; foreground color (yellow)


;; commands / keybindings
;; (setq term-unbind-key-list '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>"))

(setq term-bind-key-alist
      '(("C-c C-c" . term-interrupt-subjob)
        ("C-c C-e" . term-send-esc)
        ("C-p" . previous-line)
        ("C-n" . next-line)
        ("C-s" . isearch-forward)
        ("C-r" . isearch-backward)
        ;; ("C-m" . term-send-return)
        ;; ("C-y" . term-paste)
        ("C-v" . term-paste)
        ("M-f" . term-send-forward-word)
        ("M-b" . term-send-backward-word)
        ("M-o" . term-send-backspace)
        ("M-p" . term-send-up)
        ("M-n" . term-send-down)
        ;; ("M-M" . term-send-forward-kill-word)
        ("<C-delete>" . term-send-forward-kill-word)
        ;; ("M-N" . term-send-backward-kill-word)
        ("<C-backspace>" . term-send-backward-kill-word)
        ("M-r" . term-send-reverse-search-history)
        ("M-d" . term-send-delete-word)
        ("M-," . term-send-raw)
        ("M-." . comint-dynamic-complete)))


;; customize frame

;; (I intend to use a dedicated server (daemon) named "term" to run terminals.
;; The idea is to segregate terminal processes from file buffers and prevent
;; occasional crashes.)

(defun acg/update-term-frame (frame)
  (interactive)
  (with-selected-frame frame
    (if (string= server-name "term")
        (progn
          (set-background-color "#300A24")
          (set-frame-name "Terminal")))))

(add-to-list 'after-make-frame-functions #'acg/update-term-frame)
