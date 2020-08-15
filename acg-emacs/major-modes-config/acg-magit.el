(use-package vc
  :straight nil
  :config
  ;; Open the actual file when visiting symlink that points to a
  ;; version-controlled file
  (setq vc-follow-symlinks t))


(use-package transient
  :init
  (setq transient-history-file
        (concat acg/history-dir "transient-history.el"))
  :bind
  (:map transient-map
        ("<escape>" . transient-quit-all)))


(use-package magit
  :config
  ;; Disable Magit asking to save files
  (setq magit-save-repository-buffers nil)


  ;; Custom functions for VISITING FILES

  (defun acg/magit-diff-visit-file (file)
    "Same as `magit-diff-visit-file', but uses the default
`pop-to-buffer' (which in turn uses `display-buffer') to display
the respective file."
    (interactive (list (magit-file-at-point t t)))
    (magit-diff-visit-file--internal file nil #'pop-to-buffer))

  (defun acg/magit-diff-display-file (file)
    "Display Magit's file at point in some buffer (uses the
default `display-buffer')"
    (interactive (list (magit-file-at-point t t)))
    (magit-diff-visit-file--internal file nil #'display-buffer))

  (defun acg/magit-diff-display-file-and-next (file)
    "Same as `acg/magit-diff-display-file', but moves to the next
file before displaying file at point."
    (interactive (list (magit-file-at-point t t)))
    (magit-next-line)
    (magit-diff-visit-file--internal file nil #'display-buffer))

  (defun acg/magit-diff-display-file-and-previous (file)
    "Same as `acg/magit-diff-display-file', but moves to the
previous file before displaying file at point."
    (interactive (list (magit-file-at-point t t)))
    (magit-previous-line)
    (magit-diff-visit-file--internal file nil #'display-buffer))


  ;; Custom functions for OPENING the REMOTE REPOSITORY
  ;; Adapted from https://gist.github.com/dotemacs/9a0433341e75e01461c9

  (defun acg/parse-git-remote-url (url)
  "If necessary, convert an SSH to HTTPS git remote location."
  (if (string-match "^http" url)
      url
    (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                              "https://\\2/\\3"
                              url)))

  (defun acg/magit-open-remote-repo ()
    "Opens a remote repo URL. Prompts the user to choose a remote."
    (interactive)
    (let* ((remote-name (magit-read-remote "Choose remote repository" nil t))
           (url (magit-get "remote" remote-name "url"))
           (parsed-url (acg/parse-git-remote-url url)))
      (browse-url parsed-url)
      (message "Opening repo %s" parsed-url)))

  (defun acg/project-get-root-relative-path (&optional path)
    "Returns PATH relative to a project root."
    (let* ((path (expand-file-name (or path default-directory)))
           (root-path (expand-file-name
                       (cdr (project--find-in-directory path))))
           (root-length (length root-path))
           (rel-path (substring path root-length)))
      rel-path))

  (defun acg/magit-open-remote-dwim (&optional dir)
    "Opens a remote repo URL in the exact DIR location. Prompts the
user to choose a remote."
    (interactive)
    (let* ((file (buffer-file-name))
           (dir (expand-file-name (or dir default-directory)))
           (remote-name (magit-read-remote "Choose remote repository" nil t))
           (url (acg/parse-git-remote-url
                 (magit-get "remote" remote-name "url")))
           ;; Identify remote hosting type (e.g., GitHub, GitLab, etc.)
           (remote-hosting-type
            (cond
             ((string-match-p (regexp-quote "github") url) "GitHub")
             ((string-match-p (regexp-quote "gitlab") url) "GitLab")
             (t (completing-read "Choose remote hosting type:"
                                 '("GitHub" "GitLab"))))))
      ;; Adapt to GitLab's URL standard
      (when (string= remote-hosting-type "GitLab")
        (setq url (concat url "/-")))
      ;; Adapt to differences in file and directory URL paths
      (if file
          (setq url (concat url "/blob/" (magit-get-current-branch)
                            "/" (acg/project-get-root-relative-path file)))
        (setq url (concat url "/tree/" (magit-get-current-branch)
                          "/" (acg/project-get-root-relative-path dir))))
      ;; Perform actions
      (browse-url url)
      (message "Opening %s" url)))


  ;; Keybindings

  (mapc               ; Programatically remap M-[1-4] for all desired mode-maps
   (lambda (mode-map)
     (define-key (symbol-value mode-map) (kbd "<C-tab>") nil)
     (define-key (symbol-value mode-map) (kbd "M-1") nil)
     (define-key (symbol-value mode-map) (kbd "M-2") nil)
     (define-key (symbol-value mode-map) (kbd "M-3") nil)
     (define-key (symbol-value mode-map) (kbd "M-4") nil)
     (define-key (symbol-value mode-map) (kbd "C-c 1") 'magit-section-show-level-1-all)
     (define-key (symbol-value mode-map) (kbd "C-c 2") 'magit-section-show-level-2-all)
     (define-key (symbol-value mode-map) (kbd "C-c 3") 'magit-section-show-level-3-all)
     (define-key (symbol-value mode-map) (kbd "C-c 4") 'magit-section-show-level-4-all))
   '(magit-status-mode-map magit-diff-mode-map magit-process-mode-map))

  :bind
  (("C-x g" . magit-status)
   ("C-M-g" . magit-status)
   :map magit-status-mode-map
   ("<C-tab>" . nil)
   ("C-c <tab>" . magit-section-cycle)
   :map magit-process-mode-map
   ("<C-tab>" . nil)
   ("C-c <tab>" . magit-section-cycle)
   :map magit-log-mode-map
   ([escape] . magit-kill-this-buffer)
   ("C-w" . magit-kill-this-buffer)
   ("q" . magit-kill-this-buffer)
   ("<C-tab>" . nil)
   ("C-c <tab>" . magit-section-cycle)
   :map magit-mode-map
   ([escape] . keyboard-quit)
   ("C-w" . magit-kill-this-buffer)
   ("q" . magit-kill-this-buffer)
   ("<C-up>" . magit-section-backward-sibling)
   ("<C-down>" . magit-section-forward-sibling)
   :map magit-file-section-map
   ("<return>" . acg/magit-diff-visit-file)
   ("<C-return>" . acg/magit-diff-display-file)
   ("<S-return>" . acg/magit-diff-display-file-and-next)
   ("<C-S-return>" . acg/magit-diff-display-file-and-previous))
  :commands
  (acg/magit-open-remote-repo
   acg/magit-open-remote-dwim))

(use-package with-editor
  :bind
  (:map with-editor-mode-map
        ("C-w" . with-editor-cancel)
        ("C-s" . with-editor-finish)))


;; Provide commit message guidelines/feedback when committing;
;; taken from https://protesilaos.com/dotemacs

(use-package git-commit
  :after magit
  :custom
  (git-commit-fill-column 72)
  (git-commit-summary-max-length 50)
  (git-commit-known-pseudo-headers
   '("Signed-off-by"
     "Acked-by"
     "Modified-by"
     "Cc"
     "Suggested-by"
     "Reported-by"
     "Tested-by"
     "Reviewed-by"))
  (git-commit-style-convention-checks
   '(non-empty-second-line
     overlong-summary-line)))

;; Show word-granularity differences within diff hunks
(use-package magit-diff
  :straight nil
  :after magit
  :config
  (setq magit-diff-refine-hunk t))

;; Packages for git-related file major modes
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package gitattributes-mode)


;; Make Emacs work with ssh-ident
(use-package ssh-ident
  :straight (:host github :repo "arthurcgusmao/emacs-ssh-ident")
  :init (acg/add-to-env-path "~/.local/bin"))
