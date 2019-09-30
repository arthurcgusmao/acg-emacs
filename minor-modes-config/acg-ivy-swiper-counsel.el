(require 'ivy)
(require 'swiper)
(require 'counsel)
(require 'smex)

(ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)

;; max size the minibuffer can grow up to
(setq ivy-height 25)

;; keybindings
(global-set-key (kbd "C-f") 'swiper-isearch)
;; @todo: set C-f to restart search when in swiper
(global-set-key (kbd "C-S-F") 'swiper-all)
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

;; makes ESC quit minibuffer
(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)

;; old isearch keybindings
;; (define-key isearch-mode-map "\C-f" 'isearch-forward)
;; (define-key isearch-mode-map "\C-g" 'isearch-repeat-forward)
;; (define-key isearch-mode-map (kbd "C-S-G") 'isearch-repeat-backward)
;; (define-key isearch-mode-map (kbd "C-S-V") 'isearch-yank-kill)

