;; (use-package ivy
;;   :config
;;   ;; (setq ivy-use-virtual-buffers t)

;;   ;; max size the minibuffer can grow up to
;;   (setq ivy-height 16)
;;   ;; configure regular expression of the search
;;   (setq ivy-re-builders-alist
;;         '((swiper-isearch . regexp-quote)
;;           (t . ivy--regex-ignore-order)))
;;   ;; remove initial ^ from search
;;   (setq ivy-initial-inputs-alist nil)
;;   ;; do not quit the minibuffer when deletion error happens
;;   (setq ivy-on-del-error-function #'ignore)

;;   ;; "IVY HERE" set of functions
;;   ;; ---------------------------
;;   ;; Set of functions that run external commands on the directory of the file
;;   ;; the user was browsing on.

;;   (defun acg/ivy-get-browsing-location ()
;;     "Returns the directory path of the last ivy selection. Meant
;; to be called indirectly during an ivy minibuffer search."
;;     (expand-file-name
;;      (file-name-directory
;;       (concat (ivy-state-directory ivy-last)
;;               (ivy-state-current ivy-last)))))

;;   (defun acg/ivy-here--magit-status ()
;;     "Opens a magit-status buffer where the minibuffer was
;; browsing on and quits the minibuffer."
;;     (interactive)
;;     (ivy-quit-and-run
;;       (magit-status-setup-buffer (acg/ivy-get-browsing-location))))

;;   (defun acg/ivy-here--dired ()
;;     "Opens a dired buffer where the minibuffer was browsing on
;; and quits the minibuffer."
;;     (interactive)
;;     (ivy-quit-and-run (dired (acg/ivy-get-browsing-location))))

;;   (defun acg/ivy-here--grep-vc-or-dir ()
;;     "Performs an RG search on the project (or directory, if not a
;; project) where the minibuffer was browsing on and quits the
;; minibuffer."
;;     (interactive)
;;     (ivy-quit-and-run
;;       (let ((default-directory (acg/ivy-get-browsing-location)))
;;         (call-interactively 'prot/grep-vc-or-dir))))

;;   (defun acg/ivy-here--counsel-rg ()
;;     "Performs an RG search on the project (or directory, if not a
;; project) where the minibuffer was browsing on and quits the
;; minibuffer."
;;     (interactive)
;;     (ivy-quit-and-run
;;       (let ((default-directory (acg/ivy-get-browsing-location)))
;;         (call-interactively 'counsel-rg))))

;;   (defun acg/ivy-alt-done-all ()
;;     ""
;;     (interactive)
;;     (let ((compls (all-completions "" (ivy-state-collection ivy-last))))
;;       (mapc #'find-file-noselect (cdr compls))
;;       (find-file (car compls)))

;;     (let (alt-done-fn)
;;       (cond ((or arg (ivy--prompt-selected-p))
;;              (ivy-immediate-done))
;;             ((setq alt-done-fn (ivy-alist-setting ivy-alt-done-functions-alist))
;;              (funcall alt-done-fn))
;;             (t
;;              (ivy-done))))
;;     )


;;   :bind
;;   (:map ivy-minibuffer-map
;;    ;; Makes ESC quit minibuffer
;;    ;; (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit) ; quit or deselect text
;;    ([escape] . abort-recursive-edit) ; quit right away
;;    :map ivy-minibuffer-map
;;    ("S-SPC" . nil)
;;    ("<S-return>" . ivy-immediate-done)
;;    ("<C-return>" . ivy-restrict-to-matches)
;;    ("<return>" . ivy-alt-done)
;;    ("TAB" . ivy-partial)
;;    ("C-x g" . acg/ivy-here--magit-status)
;;    ("C-M-g" . acg/ivy-here--magit-status)
;;    ("C-x d" . acg/ivy-here--dired)
;;    ("C-M-d" . acg/ivy-here--dired)
;;    ("M-s g" . acg/ivy-here--grep-vc-or-dir)
;;    ("M-f" . acg/ivy-here--counsel-rg))
;;   :hook (after-init . ivy-mode))


;; (use-package counsel
;;   :after ivy
;;   :config
;;   (advice-add 'counsel-rg :before #'acg/with-marked-input)
;;   (advice-add 'counsel-rg :around #'acg/with-thing-at-point)
;;   :bind  (("C-o" . counsel-find-file)
;;    ("M-f" . counsel-rg)
;;    ("C-S-O" . counsel-recentf)
;;    ("C-b" . counsel-switch-buffer)
;;    ("M-x" . counsel-M-x)
;;    ("<f1> f" . counsel-describe-function)
;;    ("<f1> v" . counsel-describe-variable)
;;    ("<f1> l" . counsel-find-library)
;;    ("<f2> i" . counsel-info-lookup-symbol)
;;    ("<f2> u" . counsel-unicode-char)
;;    :map minibuffer-local-map
;;    ("C-r" . counsel-minibuffer-history))
;;   :hook (ivy-mode-hook . counsel-mode))


;; (use-package swiper
;;   :config
;;   (defun acg/swiper-thing-at-point-or-isearch (arg)
;;     "Calls swiper or isearch-forward (if ARG is non-nil) with
;; thing/symbol at point."
;;     (interactive "P")
;;     (if arg
;;         (progn (isearch-forward))
;;       (swiper-thing-at-point)))

;;   ;; preselect input
;;   (advice-add 'swiper-thing-at-point :before #'acg/with-marked-input)
;;   (advice-add 'swiper-all-thing-at-point :before #'acg/with-marked-input)
;;   :bind
;;   (("C-f" . acg/swiper-thing-at-point-or-isearch)
;;    ("C-S-F" . swiper-all-thing-at-point)))
;;    ;; @todo: set C-f to restart search when in swiper


;; ;; @todo: use variable `isearch-repeat-on-direction-change' in Emacs >= 28.1


;; ;; Smartly sort minibuffer list of candidates based on history
;; (use-package prescient
;;   :config
;;   (setq prescient-save-file (concat acg/history-dir "prescient-save.el"))
;;   (setq prescient-filter-method '(literal regexp))
;;   (prescient-persist-mode 1))
;; (use-package ivy-prescient
;;   :after (prescient ivy)
;;   :config
;;   (setq ivy-prescient-sort-commands '(:not swiper swiper-isearch ivy-switch-buffer counsel-find-file counsel-recentf))
;;   ;; Hack to make Ivy not use Prescient sorting method in find-file (see https://github.com/raxod502/prescient.el/issues/64)
;;   (ivy--alist-set 'ivy-sort-functions-alist #'read-file-name-internal #'ivy-sort-file-function-default)
;;   (ivy-prescient-mode 1))
;; (use-package company-prescient
;;   :after (prescient company)
;;   :config (company-prescient-mode 1))


;; ;; Show rich descriptions after each ivy candidate option
;; (use-package ivy-rich
;;   :config
;;   ; Show abbreviated filepaths
;;   (setq ivy-rich-path-style 'abbreviate)
;;   ;; Recommended setting (don't know why, it's said in the package description)
;;   (setcdr (assq t ivy-format-functions-alist)
;;           #'ivy-format-function-line)
;;   :hook (after-init . ivy-rich-mode))


;; isearch keybindings
;; (define-key overriding-terminal-local-map (kbd "S-SPC") nil) ; unbind S-SPC in isearch

;; old isearch keybindings
;; (define-key isearch-mode-map "\C-f" 'isearch-forward)
;; (define-key isearch-mode-map "\C-g" 'isearch-repeat-forward)
;; (define-key isearch-mode-map (kbd "C-S-G") 'isearch-repeat-backward)
;; (define-key isearch-mode-map (kbd "C-S-V") 'isearch-yank-kill)




;; Experimenting with Vertico
(use-package vertico
  :init
  (vertico-mode))

;; Marginalia = Ivy-rich for Vertico
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode)

  :config
  (defun marginalia-annotate-yasnippet (cand)
    ;; NOTE: Maybe there's a less indirect way to do this :/.
    (when-let* ((cand (marginalia--full-candidate cand))
                (template (alist-get cand consult-yasnippet--snippets nil nil #'string-equal))
                (key (yas--template-key template)))
      (concat " " (propertize (concat "[" key "]")
                              ;; TODO: custom face
                              'face 'font-lock-type-face))))

  (push '(yasnippet . marginalia-annotate-yasnippet) marginalia-annotators-light)
  (push '(yasnippet . marginalia-annotate-yasnippet) marginalia-annotators-heavy)
  )

(use-package emacs
  :config
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  )

(use-package consult
  :config

  ;;; Make completion-at-point work in the minibuffer

  (defun acg/consult-completion-in-region (start end collection &optional predicate)
    "Prompt for completion of region in the minibuffer if non-unique.
Use as a value for `completion-in-region-function'."
    (let* ((initial (buffer-substring-no-properties start end))
           (limit (car (completion-boundaries initial collection predicate "")))
           (metadata (completion-metadata initial collection predicate))
           (category (completion-metadata-get metadata 'category))
           (all (completion-all-completions initial collection predicate
                                            (length initial)))
           (exit-status 'finished)
           (completion
            (cond
             ((atom all) nil)
             ((and (consp all) (atom (cdr all)))
              (setq exit-status 'sole)
              (concat (substring initial 0 limit) (car all)))
             (t (let ((enable-recursive-minibuffers t))
                  (if (eq category 'file)
                      (read-file-name "Completion: "
                                      (file-name-directory initial)
                                      initial t
                                      (file-name-nondirectory initial)
                                      predicate)
                    (completing-read
                     ;; @acg: I have modified the line below to add a space after
                     ;; the original text, so that typing the selection of choice
                     ;; works better with orderless
                     "Completion: " collection predicate t (concat initial " "))))))))
      (if (null completion)
          (progn (message "No completion") nil)
        (delete-region start end)
        (insert (substring-no-properties completion))
        (when-let ((exit (plist-get completion-extra-properties :exit-function)))
          (funcall exit completion exit-status))
        t)))

  (defun acg/consult-completion-at-point ()
    (interactive)
    (let ((completion-in-region-function #'acg/consult-completion-in-region))
      (completion-at-point)))


  ;;; Improve isearch, grep, search by line commands

  (defun acg/consult-ripgrep-project (&optional initial)
    "Same as `consult-ripgrep' but defaults to project
directory."
    (interactive)
    (consult--grep
     "rg"
     "rg --null --line-buffered --color=ansi --max-columns=1000 --smart-case --no-heading --line-number . -e ARG OPTS"
     (rg-project-root default-directory) initial))

  (advice-add 'consult-line :around #'acg/with-thing-at-point)
  (advice-add 'consult-line :before #'acg/with-marked-input)
  (advice-add 'acg/consult-ripgrep-project :around #'acg/with-thing-at-point)
  (advice-add 'acg/consult-ripgrep-project :before #'acg/with-marked-input)

  :bind
  ;; ("C-M-i" . acg/corfu-orderless-completion-at-point)
  (("C-f" . consult-line)
   ("M-f" . acg/consult-ripgrep-project)
   ("C-M-S-i" . acg/consult-completion-at-point)))

;; (use-package orderless
;;   :ensure t
;;   :custom (completion-styles '(orderless))
;;   :config
;;   ;; (setq orderless-style-dispatchers '())
;;   )
(use-package orderless
  :init
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles . (partial-completion))))))


(use-package corfu
  :config
  (corfu-global-mode 1)

  ;;; Completion with orderless and ability to type text for selection

  ;; This section implements functions to make the functionality described
  ;; above work.

  (defun acg/corfu-completion-at-point ()
    "Adds a space after calling `completion-at-point' so
orderless can do its magic."
    (interactive )
    (when (completion-at-point)
      (insert " ")))

  (defun acg/completion--in-region--advice (start end collection &optional predicate)
    (setq acg/completion-in-region--original-text `(,start ,end ,(buffer-substring start end))))
  (advice-add 'completion--in-region :after #'acg/completion--in-region--advice)
  ;; (advice-remove 'completion--in-region 'acg/completion--in-region--advice)

  (defun acg/corfu-abort ()
    (interactive)
    ;; Replace region with original text
    (let ((beg (nth 0 acg/completion-in-region--original-text))
          (end (nth 1 acg/completion-in-region--original-text))
          (txt (nth 2 acg/completion-in-region--original-text)))
      (when (> (point) end)
        (setq end (point)))
      (delete-region beg end)
      (insert txt))
    ;; Call regular abort function
    (corfu-abort))


  ;;; Smart TAB behavior

  (defun acg/indent-or-corfu-complete-at-point ()
  "Indent the current line or region (using function supplied as argument), or
complete the common part."
  (interactive)
  (cond
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((let ((old-point (point))
          (old-tick (buffer-chars-modified-tick))
          (tab-always-indent t))
      (call-interactively #'indent-for-tab-command)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick))
                 ;; do not run complete-common when blank chars before is
                 (not (or (eq (char-before) 10)                         ; newline
                          (eq (char-before) 32)                         ; whitespace
                          (eq (char-before) 41)                         ; )
                          (eq (char-before) 93)                         ; ]
                          (eq (char-before) 125))))                     ; }
        (acg/corfu-completion-at-point))))))


  :bind
  (("TAB" . acg/indent-or-corfu-complete-at-point)
   :map corfu-map
   ("<escape>" . acg/corfu-abort)))




;; Test and debug
;; (compl)


;; (setq acg/anticipate-deactivate-mark-flag t)

;; ;; (defun acg/maybe-anticipate-deselection (&rest args)
;; ;;   (when (and delete-selection-mode
;; ;;              (region-active-p)
;; ;;              acg/anticipate-deselection-flag)
;; ;;       (setq deactivate-mark t)
;; ;;     (funcall orig-fun args)))

;; (defun acg/maybe-anticipate-deactivate-mark ()
;;   (when (and delete-selection-mode
;;              (region-active-p)
;;              acg/anticipate-deactivate-mark-flag)
;;     (deactivate-mark)
;;     (setq acg/anticipate-deactivate-mark-flag nil)
;;     (message this-command)))

;; (advice-add 'minibuffer-keyboard-quit :before #'acg/maybe-anticipate-deactivate-mark)
