(defun acg/swiper-thing-at-point-or-isearch (arg)
  "Calls swiper or isearch-forward (if ARG is non-nil) with
thing/symbol at point."
  (interactive "P")
  (if arg
      (progn (isearch-forward))
    (swiper-thing-at-point)))


(use-package ivy
  :config
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

  ;; "IVY HERE" set of functions
  ;; ---------------------------
  ;; Set of functions that run external commands on the directory of the file
  ;; the user was browsing on.

  (defun acg/ivy-get-browsing-location ()
    "Returns the directory path of the last ivy selection. Meant
to be called indirectly during an ivy minibuffer search."
    (expand-file-name
     (file-name-directory
      (concat (ivy-state-directory ivy-last)
              (ivy-state-current ivy-last)))))

  (defun acg/ivy-here--magit-status ()
    "Opens a magit-status buffer where the minibuffer was
browsing on and quits the minibuffer."
    (interactive)
    (ivy-quit-and-run
      (magit-status-setup-buffer (acg/ivy-get-browsing-location))))

  (defun acg/ivy-here--dired ()
    "Opens a dired buffer where the minibuffer was browsing on
and quits the minibuffer."
    (interactive)
    (ivy-quit-and-run (dired (acg/ivy-get-browsing-location))))

  (defun acg/ivy-here--grep-vc-or-dir ()
    "Performs an RG search on the project (or directory, if not a
project) where the minibuffer was browsing on and quits the
minibuffer."
    (interactive)
    (ivy-quit-and-run
      (let ((default-directory (acg/ivy-get-browsing-location)))
        (call-interactively 'prot/grep-vc-or-dir))))


  :bind
  (:map ivy-minibuffer-map
   ;; Makes ESC quit minibuffer
   ;; (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit) ; quit or deselect text
   ([escape] . abort-recursive-edit) ; quit right away
   :map ivy-minibuffer-map
   ("S-SPC" . nil)
   ("<S-return>" . ivy-immediate-done)
   ("<C-return>" . ivy-restrict-to-matches)
   ("<return>" . ivy-alt-done)
   ("TAB" . ivy-partial)
   ("C-x g" . acg/ivy-here--magit-status)
   ("C-x d" . acg/ivy-here--dired)
   ("M-s g" . acg/ivy-here--grep-vc-or-dir))
  :hook (after-init . ivy-mode))


(use-package counsel
  :after ivy
  :bind  (("C-o" . counsel-find-file)
   ("M-f" . counsel-rg)
   ("C-S-O" . counsel-recentf)
   ("C-b" . counsel-switch-buffer)
   ("M-x" . counsel-M-x)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history))
  :hook (ivy-mode-hook . counsel-mode))


(use-package swiper
  :config
  ;; preselect input
  (advice-add 'swiper-thing-at-point :before #'acg/with-marked-input)
  (advice-add 'swiper-all-thing-at-point :before #'acg/with-marked-input)
  :bind
  (("C-f" . acg/swiper-thing-at-point-or-isearch)
   ("C-S-F" . swiper-all-thing-at-point)))
   ;; @todo: set C-f to restart search when in swiper


(use-package smex)


;; Show rich descriptions after each ivy candidate option
(use-package ivy-rich
  :config
  ; Show abbreviated filepaths
  (setq ivy-rich-path-style 'abbreviate)
  ;; Recommended setting (don't know why, it's said in the package description)
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  :hook (after-init . ivy-rich-mode))


;; isearch keybindings
;; (define-key overriding-terminal-local-map (kbd "S-SPC") nil) ; unbind S-SPC in isearch

;; old isearch keybindings
;; (define-key isearch-mode-map "\C-f" 'isearch-forward)
;; (define-key isearch-mode-map "\C-g" 'isearch-repeat-forward)
;; (define-key isearch-mode-map (kbd "C-S-G") 'isearch-repeat-backward)
;; (define-key isearch-mode-map (kbd "C-S-V") 'isearch-yank-kill)
