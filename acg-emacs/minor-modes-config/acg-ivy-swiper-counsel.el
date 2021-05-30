;;; General completion configurations

(use-package emacs
  :straight nil
  :config
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t))

(use-package orderless
  :init
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles . (partial-completion))))))


;;; Traditional search within buffer

(use-package isearch
  :straight nil
  :config
  (setq isearch-repeat-on-direction-change t)
  :bind
  (:map isearch-mode-map
        ("<escape>" . isearch-abort)
        ("C-r" . consult-isearch)
        ("C-s" . nil)
        ("<up>" . isearch-repeat-backward)
        ("<down>" . isearch-repeat-forward)))

(use-package ctrlf
  :config
  (defun acg/smart-ctrlf-forward (&optional initial-contents)
    (interactive)
    ;; Mark input -- `acg/with-marked-input' doesn't work due to `ctrlf' weird input method
    (run-with-idle-timer 0.05 nil (lambda () (mark-whole-buffer)))
    (ctrlf-forward 'regexp nil (or initial-contents "")))

  (advice-add 'acg/smart-ctrlf-forward :around #'acg/with-thing-at-point)

  ;; Define keybindings (ctrlf uses this variable instead of a keymap)
  (append-to-list 'ctrlf-minibuffer-bindings
                  '(("<up>" . ctrlf-backward-regexp)
                    ("<down>" . ctrlf-forward-regexp)))

  (ctrlf-mode +1)
  :bind
  ("M-s f" . acg/smart-ctrlf-forward))



;;; Minibuffer vertical completion

(use-package vertico
  :init
  (vertico-mode 1)
  :config
  ;; Disable features that don't go well w/ Vertico
  (advice-add #'vertico--setup :after
              (lambda (&rest _)
                (setq-local completion-auto-help nil
                            completion-show-inline-help nil)))

  (defun acg/minibuffer-smart-backspace ()
    "Same as <backspace> but acts differently when finding files."
    (interactive)
    (if (and minibuffer-completing-file-name
             (= (char-before) ?/))
        (acg/backward-kill-word)
      (call-interactively 'delete-backward-char)))

  (defun acg/vertico-smart-exit ()
    "Same as `vertico-exit' but enters directory when finding files."
    (interactive)
    (if (and minibuffer-completing-file-name
             (string= (substring (vertico--candidate) -1) "/"))
        (vertico-insert)
      (call-interactively 'vertico-exit)))


  ;;; Make commands use the current minibuffer input

  ;; Very similar to Embark, but I created these custom functions
  ;; because Embark required an extra keybinding to be assigned.

  (defun acg/vertico--candidate-dir ()
    "Gets the default directory of the current candidate."
    (let ((cand (vertico--candidate)))
      (cond
       (minibuffer-completing-file-name
        (file-name-directory
         (expand-file-name cand)))
       ((get-buffer cand)
        (with-current-buffer cand
          default-directory))
       (t
        (file-name-directory
         (expand-file-name cand))))))

  (defun acg/vertico-embark--magit ()
    (interactive)
    (embark--quit-and-run
     'magit-status
     (locate-dominating-file (acg/vertico--candidate-dir) ".git")))

  (defun acg/vertico-embark--dired ()
    (interactive)
    (embark--quit-and-run
     'dired
     (acg/vertico--candidate-dir)))

  (defun acg/vertico-embark--consult-ripgrep ()
    (interactive)
    (embark--quit-and-run
     'consult-ripgrep
     (locate-dominating-file (acg/vertico--candidate-dir) ".git")))


  :bind
  (:map vertico-map
        ("<S-return>" . vertico-exit-input)
        ("<return>" . acg/vertico-smart-exit)
        ("<backspace>" . acg/minibuffer-smart-backspace)

        ;; ("<C-return>" . vert) ; @todo: see how to narrow selection in vertico -- dual of ivy-restrict-to-matches

        ("C-x g" . acg/vertico-embark--magit)
        ("C-M-g" . acg/vertico-embark--magit)
        ("C-x d" . acg/vertico-embark--dired)
        ("C-M-d" . acg/vertico-embark--dired)
        ("M-f" . acg/vertico-embark--consult-ripgrep)))


;; Minibuffer metadata enhancement
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode)
  ;; @todo: replace ivy-yasnippet
  )




(use-package consult
  :config
  (setq consult-line-start-from-top nil)
  (setq consult-line-point-placement 'match-end)

  ;; @todo: Use `vertico--goto' to make vertico selected candidate start from
  ;; the current line!

  ;; Try and fix consult line 'wrapped around' behavior

  ;; (defun consult--line-candidates (top)
  ;; "Return list of line candidates; start from top if TOP non-nil."
  ;; (consult--forbid-minibuffer)
  ;; (consult--fontify-all)
  ;; (let* ((default-cand)
  ;;        (candidates)
  ;;        (line (line-number-at-pos (point-min) consult-line-numbers-widen))
  ;;        (curr-line (line-number-at-pos (point) consult-line-numbers-widen)))
  ;;   (consult--each-line beg end
  ;;     (let ((str (consult--buffer-substring beg end)))
  ;;       (unless (string-blank-p str)
  ;;         (push (consult--location-candidate str (point-marker) line) candidates)
  ;;         (when (and (not default-cand) (>= line curr-line))
  ;;           (setq default-cand candidates)))
  ;;       (setq line (1+ line))))
  ;;   (unless candidates
  ;;     (user-error "No lines"))
  ;;   (setq acg/consult-line--offset (length (cdr default-cand)))
  ;;   (nreverse
  ;;    (if top
  ;;        candidates
  ;;      (let ((before (cdr default-cand)))
  ;;        (setcdr default-cand nil)
  ;;        (nconc before candidates))))))

;;   (defun consult-line (&optional initial start)
;;   "Search for a matching line and jump to the line beginning.

;; The default candidate is a non-empty line closest to point.
;; This command obeys narrowing. Optional INITIAL input can be provided.
;; The search starting point is changed if the START prefix argument is set.
;; The symbol at point and the last `isearch-string' is added to the future history."
;;   (interactive (list nil (not (not current-prefix-arg))))
;;   (let ((candidates (consult--with-increased-gc
;;                      (consult--line-candidates
;;                       (not (eq start consult-line-start-from-top))))))
;;     (consult--read
;;      (cdr candidates)
;;      :prompt "Go to line: "
;;      :annotate (consult--line-prefix)
;;      :category 'consult-location
;;      :sort nil
;;      :require-match t
;;      ;; Always add last isearch string to future history
;;      :add-history (list (thing-at-point 'symbol) isearch-string)
;;      :history '(:input consult--line-history)
;;      :lookup #'consult--line-match
;;      ;; :default (car candidates)
;;      :default nil
;;      ;; Add isearch-string as initial input if starting from isearch
;;      :initial (or initial
;;                   (and isearch-mode (prog1 isearch-string (isearch-done))))
;;      :state (consult--jump-state)))
;;   (vertico--goto 3))

  (defun acg/consult-line (&optional arg)
    "Runs `consult-line' with `vertico-cycle' ON. Also, switches to
`consult-outline' if prefix argument provided."
    (interactive "P")
    (let ((fun (if arg 'consult-outline 'consult-line))
          (vertico-cycle t))
      (call-interactively fun)))


  ;;;; Make completion-at-point work in the minibuffer

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


  ;;;; Improve isearch, grep, search by line commands

  (defun acg/consult-ripgrep-project (&optional initial)
    "Same as `consult-ripgrep' but defaults to project
directory."
    (interactive)
    ;; Mark input compensating for the initial #
    (run-with-idle-timer
     0.05 nil (lambda ()
                (push 'S-end unread-command-events)
                (push 'right unread-command-events)
                (push 'home unread-command-events)))
    ;; Call original command
    (consult--grep
     "rg"
     "rg --null --line-buffered --color=ansi --max-columns=1000 --smart-case --no-heading --line-number . -e ARG OPTS"
     (rg-project-root default-directory) initial))

  ;; Preselect input
  (advice-add 'consult-line :around #'acg/with-thing-at-point)
  (advice-add 'consult-line :before #'acg/with-marked-input)
  ;; (advice-remove 'consult-line 'acg/with-thing-at-point)
  ;; (advice-remove 'consult-line 'acg/with-marked-input)

  (advice-add 'acg/consult-ripgrep-project :around #'acg/with-thing-at-point)


  :bind
  ;; ("C-M-i" . acg/corfu-orderless-completion-at-point)
  (("C-f" . acg/consult-line)
   ("C-S-F" . consult-multi-occur) ;; @todo: default to all buffers, do not ask
   ("M-f" . acg/consult-ripgrep-project)
   ;; @todo: set C-f to restart search when in consult-line & others
   ("C-M-S-i" . acg/consult-completion-at-point)
   ("C-S-O" . consult-recent-file) ; @todo: disable preview of files
   ("M-g g" . consult-goto-line)
   (:map minibuffer-local-map
         ("C-r" . consult-history))
   (:map consult-isearch-map
         ("<up>" . consult-isearch-reverse)
         ("<down>" . consult-isearch-forward))))




;;; `completion-at-point' packages

(use-package corfu
  :config
  ;;; Completion with orderless and ability to type text for selection

  ;; Add space to completion so orderless can do its magic

  (defun acg/corfu-complete ()
    "Adds a space after calling `completion-at-point' so
orderless can do its magic."
    (interactive)
    (when (completion-at-point) (insert " ")))


  ;; Make corfu-abort remove the extra space added above

  (defvar acg/completion-in-region--original-input nil
    "Store START, END, and BUFFER-SUBSTRING information of the
last call to `completion-in-region'.")

  (defun acg/completion--in-region--save-input (start end collection &optional predicate)
    "Updates the value of `acg/completion-in-region--original-input'.
To be used as advice after `completion--in-region'."
    (setq acg/completion-in-region--original-input
          `(,start ,end ,(buffer-substring start end))))

  (advice-add 'completion--in-region :after #'acg/completion--in-region--save-input)
  ;; (advice-remove 'completion--in-region 'acg/completion--in-region--advice)

  (defun acg/corfu-abort ()
    (interactive)
    ;; Replace region with original text
    (let ((beg (nth 0 acg/completion-in-region--original-input))
          (end (nth 1 acg/completion-in-region--original-input))
          (txt (nth 2 acg/completion-in-region--original-input)))
      (when (> (point) end)
        (setq end (point)))
      (delete-region beg end)
      (insert txt))
    ;; Call regular abort function
    (corfu-abort))

  (corfu-global-mode +1)
  :bind
  (:map corfu-map
        ("<tab>" . acg/corfu-complete)
        ("<escape>" . acg/corfu-abort)))
