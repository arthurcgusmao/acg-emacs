(use-package python
  :straight nil
  :config
  (setq python-indent-offset 4)
  ;; Customized behavior of indent functions
  (advice-add 'python-indent-shift-left :around #'acg/with-expanded-region-to-whole-lines)
  (advice-add 'python-indent-shift-right :around #'acg/with-expanded-region-to-whole-lines)


  (defun acg/python-shell-send-region (beg end)
    "Similar to `python-shell-send-region' but displays output if
any, similar to what a Jupyter REPL would do."
    (interactive "r")
    (python-shell-send-string
     (buffer-substring beg end)))


  ;;; CODE FORMATTERS and related

  ;;;; CODE ONLY (not docstrings)

  ;; Facilitate use of autopep8 inside Emacs.
  ;; https://pypi.org/project/autopep8/ (Install w/ `pip install autopep8')
  (defun acg/autopep8-fill-paragraph ()
    (interactive)
    (acg/call-process-region
     "autopep8"
     `("--max-line-length" ,(number-to-string fill-column) "-a" "-a" "-")
     nil t 'mark-paragraph))
  (defun acg/autopep8-buffer ()
    (interactive)
    (acg/call-process-region
     "autopep8" '("-") t nil 'mark-whole-buffer))

  ;; Facilitate use of black inside Emacs.
  ;; https://github.com/psf/black (Install w/ `pip install black')
  (defun acg/py-black-fill-paragraph ()
    (interactive)
    (acg/call-process-region
     "xargs"
     `("-0" "black" "--line-length" ,(number-to-string fill-column) "--code")
     nil t 'mark-paragraph))
  (defun acg/py-black-buffer ()
    (interactive)
    (acg/call-process-region
     "xargs"
     `("-0" "black" "--line-length" ,(number-to-string fill-column) "--code")
     t nil 'mark-whole-buffer))

  ;;;; DOCSTRINGS

  ;; Configure default behavior of fill-paragraph
  (setq python-fill-docstring-style 'pep-257-nn)

  ;; Facilitate use of docformatter inside Emacs.
  ;; https://github.com/myint/docformatter (Install w/ `pip install docformatter')
  (defun acg/py-docformatter-fill-paragraph ()
    (interactive)
    (acg/call-process-region
     "docformatter" '("-") nil t 'er/mark-outside-python-string))
  (defun acg/py-docformatter-buffer ()
    (interactive)
    (acg/call-process-region
     "docformatter" '("-") t nil 'mark-whole-buffer))

  ;;;; Putting it all together

  (defun acg/python-fill-paragraph ()
    "My version of python-fill-paragraph."
    (interactive)
    (cond ((python-info-docstring-p) (acg/py-docformatter-fill-paragraph))
          (t (acg/autopep8-fill-paragraph))))

  (define-key python-mode-map (kbd "M-p") 'acg/python-fill-paragraph)


  ;; Other configurations to be run after python-mode is loaded in a buffer

  (defun acg/after-python-hook ()
    "Configs to be added to PYTHON-MODE-HOOK."
    ;; Config for when using tabs instead of spaces (usually not the case)
    (setq tab-width 4)
    ;; Fix newline indentation (indent according to previous blank line)
    (setq acg/electric-indent-newline-as-previous-if-blank t))

  (defun acg/python-shell-send-string (str &optional beg end)
    (python-shell-send-string str))

  ;; The functions below are just sending the code to the shell
  ;; ("invisibly"), but not printing it. @TODO: create a new function
  ;; that prints the results of the last variable, so that it works
  ;; like the Jupyter REPL.
  (define-key python-mode-map (kbd "C-c C-b") (acg/eval-with 'acg/python-shell-send-region 'mark-whole-buffer))
  (define-key python-mode-map (kbd "C-c C-p") (acg/eval-with 'acg/python-shell-send-region 'mark-page))
  (define-key python-mode-map (kbd "C-c C-c") (acg/eval-with 'acg/python-shell-send-region 'acg/mark-dwim))
  (define-key python-mode-map (kbd "C-c C-l") (acg/eval-with 'acg/python-shell-send-string 'acg/expand-region-to-whole-lines 'acg/unindent-add-last-var))

  (defun acg/run-python-project-root ()
    "Runs `run-python' at the project's root directory.
Prompts for a directory if not in project."
    (interactive)
    (let ((default-directory
           (or (project-root (project-current nil))
               (read-directory-name "Not in project, choose dir: "
                                    default-directory nil t))))
      (call-interactively 'run-python)))

  :hook
  (python-mode . acg/after-python-hook)

  ;; KEYBINDINGS
  :bind
  (:map python-mode-map
        ("M-<" . python-indent-shift-left)
        ("M->" . python-indent-shift-right)
        ("<f7>" . python-shell-switch-to-shell)
        ("<f8>" . python-shell-send-buffer)
        ("C-c y y" . run-python)
        ("C-c y p" . acg/run-python-project-root)
        ))
