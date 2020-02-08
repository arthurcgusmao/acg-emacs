(require 'ivy)
(require 'swiper)
(require 'counsel)
(require 'smex)

(ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)

;; max size the minibuffer can grow up to
(setq ivy-height 16)

;; configure regular expression of the search
(setq ivy-re-builders-alist
      '((swiper-isearch . regexp-quote)
        (t . ivy--regex-ignore-order)))

;; remove initial ^ from search
(setq ivy-initial-inputs-alist nil)

;; do not quit the minibuffer when deletion error happens
(setq ivy-on-del-error-function #'ignore)


(defun acg/swiper-thing-at-point-or-isearch (arg)
  "Calls swiper or isearch-forward (if ARG is non-nil) with
thing/symbol at point."
  (interactive "P")
  (if arg
      (progn (isearch-forward))
    (swiper-thing-at-point)))

;; preselect input
(advice-add 'swiper-thing-at-point :before #'acg/with-marked-input)
(advice-add 'swiper-all-thing-at-point :before #'acg/with-marked-input)

;; Show rich descriptions after each ivy candidate option
(use-package ivy-rich
  :ensure t
  :config
  ; Show abbreviated filepaths
  (setq ivy-rich-path-style 'abbreviate)
  ;; Recommended setting (don't know why, it's said in the package description)
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  :hook (after-init . ivy-rich-mode))


;; keybindings
(global-set-key (kbd "C-f") 'acg/swiper-thing-at-point-or-isearch)
;; @todo: set C-f to restart search when in swiper
(global-set-key (kbd "C-S-F") 'swiper-all-thing-at-point)
(global-set-key (kbd "C-o") 'counsel-find-file)
(global-set-key (kbd "C-S-O") 'counsel-recentf)
(global-set-key (kbd "C-b") 'counsel-switch-buffer)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key ivy-minibuffer-map (kbd "S-SPC") nil)
(define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "<S-return>") 'ivy-restrict-to-matches)
(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)

;; makes ESC quit minibuffer
;; (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit) ; quit or deselect text
(define-key ivy-minibuffer-map [escape] 'abort-recursive-edit) ; quit right away

;; isearch keybindings
;; (define-key overriding-terminal-local-map (kbd "S-SPC") nil) ; unbind S-SPC in isearch

;; old isearch keybindings
;; (define-key isearch-mode-map "\C-f" 'isearch-forward)
;; (define-key isearch-mode-map "\C-g" 'isearch-repeat-forward)
;; (define-key isearch-mode-map (kbd "C-S-G") 'isearch-repeat-backward)
;; (define-key isearch-mode-map (kbd "C-S-V") 'isearch-yank-kill)

