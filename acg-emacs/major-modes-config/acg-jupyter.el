(use-package jupyter)
(use-package expand-region)


;; @todo: create pull request to jupyter package with changes in this page

;; new function
(defun jupyter-repl-display-buffer ()
  "Display (but not switch to) the REPL buffer of the `jupyter-current-client'."
  (interactive)
  (if jupyter-current-client
      (jupyter-with-repl-buffer jupyter-current-client
        (goto-char (point-max))
        (display-buffer (current-buffer)))
    (error "Buffer not associated with a REPL, see `jupyter-repl-associate-buffer'")))

;; Wrapper function to add 'display mode to jupyter-eval-string
(defun acg/jupyter-eval-string (str &optional beg end)
  "Evaluate STR using jupyter-eval-string with display mode support.
When `jupyter-repl-echo-eval-p' is 'display, automatically display
the REPL buffer without selecting it."
  (let ((req (jupyter-eval-string str beg end)))
    (when (and (eq jupyter-repl-echo-eval-p 'display)
               jupyter-current-client
               (object-of-class-p jupyter-current-client 'jupyter-repl-client))
      (jupyter-repl-display-buffer))
    req))


;; Custom Python functions

;; Open Python variable content in external app
(defun acg/jupyter-open-python-variable-external-app ()
  "Saves the content of a Python variable to a temporary file and opens
it with the default external app."
  (interactive)
  (let* ((var (read-string "Enter variable: "
                           (thing-at-point 'symbol)))
         (type (jupyter-eval (format "type(%s)" var)))
         (tempfpath (concat temporary-file-directory "emacs-jupyter---"
                            (format-time-string "%Y-%m-%d--%Hh%Mm%Ss--") var)))
    (pcase type
      ("pandas.core.frame.DataFrame"
       (setq tempfpath (concat tempfpath ".csv"))
       (jupyter-eval (format "%s.to_csv('%s')" var tempfpath))
       (acg/open-in-external-app (list tempfpath)))
      ("pandas.core.series.Series"
       (setq tempfpath (concat tempfpath ".csv"))
       (jupyter-eval (format "%s.to_csv('%s')" var tempfpath))
       (acg/open-in-external-app (list tempfpath)))
      ("numpy.ndarray"
       (setq tempfpath (concat tempfpath ".csv"))
       (jupyter-eval (format "import numpy as np; np.savetxt('%s', %s, delimiter=',')" tempfpath var))
       (acg/open-in-external-app (list tempfpath)))
      ("str"
       (setq tempfpath (concat tempfpath ".txt"))
       (jupyter-eval (format "with open('%s','w') as f: f.write(%s)" tempfpath var))
       (acg/open-in-external-app (list tempfpath)))
      ("dict"
       (setq tempfpath (concat tempfpath ".json"))
       (jupyter-eval (format "import json; f=open('%s','w'); f.write(json.dumps(%s, indent=4)); f.close()" tempfpath var))
       (acg/open-in-external-app (list tempfpath)))
      )))

;; Show value of the last variable that was assigned in Jupyter
(defun acg/add-last-var (code-str)
  "Returns a modified string by appending to CODE-STR a newline
with the last line's assigned variable (if any variable
assignment occurred). Useful for code inspection in REPLs (such
as Jupyter)."
  (let (lines last-line)
    (setq lines (split-string code-str "\n"))
    (delete "" lines)
    (setq last-line (car (last lines)))
    (if (string-match
         "^\\`\\([A-Za-z]+[A-Za-z0-9_\.]*\\) *[\+\*\/-]*=[^=].+\\'"
         last-line)
        (let* ((var-name (match-string 1 last-line)))
          (concat code-str "\n" var-name))
      code-str)))

(defun acg/unindent-add-last-var (code-str)
  "Calls the functions `acg/add-last-var' and `acg/unindent-string'."
  (acg/add-last-var (acg/unindent-string code-str)))


;; Configurations

(defun acg/jupyter-toggle-use-overlays-repl (&optional arg)
  "Toggles automatically between using overlays in the buffer
where code is and sending code to be evaluated in the REPL."
  (interactive)
  (if jupyter-eval-use-overlays
      (progn
        (setq jupyter-eval-use-overlays nil
              jupyter-repl-echo-eval-p 'display)
        (message "Jupyter overlays disabled; sending output to REPL."))
    (progn
      (setq jupyter-eval-use-overlays t
            jupyter-repl-echo-eval-p nil)
      (message "Jupyter overlays enabled."))))

(setq jupyter-eval-use-overlays nil
      jupyter-repl-echo-eval-p 'display)

;; Custom faces/visuals
(set-face-attribute 'jupyter-repl-traceback nil :background "#660000")

;; Remove prefix from overlay
(setq jupyter-eval-overlay-prefix nil)

;; Avoid y-or-n-p questions when killing REPL buffers
(defadvice jupyter-repl-kill-buffer-query-function (around auto-confirm compile activate)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
    ad-do-it))


;; Start REPL at different paths

(defun acg/jupyter-run-repl-project-root ()
  "Runs `jupyter-run-repl' at the project's root directory.
Prompts for a directory if not in project."
  (interactive)
  (let ((default-directory
          (or (project-root (project-current nil))
              (read-directory-name "Not in project, choose dir: "
                                   default-directory nil t))))
    (call-interactively 'jupyter-run-repl)))

(defun acg/jupyter-run-repl-elsewhere (&optional dir)
  "Runs `jupyter-run-repl' with DIR as the default directory.
Prompts for a directory if DIR is nil."
  (interactive)
  (let ((dir (or dir
                 (read-directory-name "Choose dir: "
                                      default-directory nil t))))
    (call-interactively 'jupyter-run-repl)))


;; Keybindings
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c j j") 'jupyter-run-repl)
  (define-key python-mode-map (kbd "C-c j p") 'acg/jupyter-run-repl-project-root)
  (define-key python-mode-map (kbd "C-c j e") 'acg/jupyter-run-repl-elsewhere)
  (define-key python-mode-map (kbd "C-c j a") 'jupyter-repl-associate-buffer))
(define-key jupyter-repl-interaction-mode-map (kbd "C-c j r") 'jupyter-repl-restart-kernel)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c j i") 'jupyter-repl-interrupt-kernel)

(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-b") (acg/eval-with 'acg/jupyter-eval-string 'mark-whole-buffer 'acg/add-last-var))
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-p") (acg/eval-with 'acg/jupyter-eval-string 'mark-page 'acg/add-last-var))
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-c") (acg/eval-with 'acg/jupyter-eval-string 'acg/mark-dwim 'acg/add-last-var))
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-l") (acg/eval-with 'acg/jupyter-eval-string 'acg/expand-region-to-whole-lines 'acg/unindent-add-last-var))
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-d") (acg/eval-with 'acg/jupyter-eval-string 'acg/mark-defun-body 'acg/unindent-add-last-var))
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-e") 'acg/jupyter-open-python-variable-external-app)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-o") nil)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-o t") 'acg/jupyter-toggle-use-overlays-repl)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-o r") 'jupyter-eval-remove-overlays)


;; To-Dos -- Functions to look into
;;     - jupyter-completion-symbol-beginning -- What I want here is to complete on `pd.DataFram...' instead of just `DataFram...'; Check to see what is the exact use of this function and if that is not currently working or is
