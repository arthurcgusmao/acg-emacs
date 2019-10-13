(require 'jupyter)
(require 'expand-region)

;; Custom eval functions

(defun acg/jupyter-eval-region-lines ()
  "Same as `jupyter-eval-region' but operates on the whole lines
that the region spans. It also compensates for indentation by
removing the lowest indentation in the region lines from all
lines before evaluating."
  (interactive)
  (save-mark-and-excursion
    (acg/expand-region-to-whole-lines)
    (jupyter-eval-string
     (acg/get-region-unindented nil (region-beginning) (region-end))
     (region-beginning) (region-end))))

(defun acg/jupyter-eval-with (func)
  "Creates and returns a function that evaluates the region
marked by FUNC. If universal argment is passed, evaluates the
region only up to the cursor position."
  (let ((func-symbol (make-symbol (concat "acg/jupyter-eval-with--"
                                          (symbol-name func)))))
    (eval `(defun ,func-symbol (&optional arg)
             "Evaluates the lines of the region marked by the
respective function. If universal argument is passed, evaluates
the region only up to the line where the cursor is."
             (interactive "P")
             (save-mark-and-excursion
               (if arg
                   (progn
                     (save-excursion
                       (,func)
                       (if (< (point) (mark))
                           (exchange-point-and-mark)))
                     (end-of-line))
                 (,func))
               (jupyter-eval-region
                (region-beginning) (region-end)))))))


;; Custom Python functions

;; Open Python variable content in external app
(defun acg/jupyter-open-python-variable-external-app ()
  "Saves the content of a Python variable to a temporary file and opens
it with the default external app."
  (interactive)
  (let* ((var (read-string "Enter variable: "
                           (thing-at-point 'symbol)))
         (type (jupyter-eval (format "type(%s)" var))))
    (pcase type
      ("pandas.core.frame.DataFrame"
       (let ((tempfpath
              (concat temporary-file-directory "emacs-jupyter--" var ".csv")))
         (jupyter-eval (format "%s.to_csv('%s')" var tempfpath))
         (acg/open-in-external-app (list tempfpath))
         ))
      ("numpy.ndarray"
       (let ((tempfpath
              (concat temporary-file-directory "emacs-jupyter--" var ".csv")))
         (jupyter-eval (format "import numpy as np; np.savetxt('%s', %s, delimiter=',')" tempfpath var))
         (acg/open-in-external-app (list tempfpath))))
      ("str"
       (let ((tempfpath
              (concat temporary-file-directory "emacs-jupyter--" var ".txt")))
         (jupyter-eval (format "with open('%s','w') as f: f.write(%s)" tempfpath var))
         (acg/open-in-external-app (list tempfpath))))
      ("dict"
       (let ((tempfpath
              (concat temporary-file-directory "emacs-jupyter--" var ".json")))
         (jupyter-eval (format "with open('%s','w') as f: f.write(json.dumps(%s, indent=4))" tempfpath var))
         (acg/open-in-external-app (list tempfpath))))
      )))


;; Configurations

(defun acg/jupyter-toggle-use-overlays-repl (&optional arg)
  "Toggles automatically between using overlays in the buffer
where code is and sending code to be evaluated in the REPL."
  (interactive)
  (if jupyter-eval-use-overlays
      (progn
        (setq jupyter-eval-use-overlays nil
              jupyter-repl-echo-eval-p t)
        (message "Jupyter overlays disabled; sending output to REPL."))
    (progn
      (setq jupyter-eval-use-overlays t
            jupyter-repl-echo-eval-p nil)
      (message "Jupyter overlays enabled."))))

(setq jupyter-eval-use-overlays t
      jupyter-repl-echo-eval-p nil)

;; Custom faces/visuals
(set-face-attribute 'jupyter-repl-traceback nil :background "#660000")

;; Remove prefix from overlay
(setq jupyter-eval-overlay-prefix nil)
;; Override default function to remove space between prefix and overlay
(defun jupyter-eval-ov--propertize (text &optional newline)
  ;; Display properties can't be nested so use the one on TEXT if available
  (if (get-text-property 0 'display text) text
    (let ((display (concat
                    ;; Add a space before a newline so that `point' stays on
                    ;; the same line when moving to the beginning of the
                    ;; overlay.
                    (if newline " \n" " ")
                    (propertize
                     (concat jupyter-eval-overlay-prefix text)
                     'face 'jupyter-eval-overlay))))
      ;; Ensure `point' doesn't move past the beginning or end of the overlay
      ;; on motion commands.
      (put-text-property 0 1 'cursor t display)
      (put-text-property (1- (length display)) (length display) 'cursor t display)
      (propertize " " 'display display))))


;; Keybindings
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c j") 'jupyter-run-repl))
(define-key jupyter-repl-interaction-mode-map (kbd "C-c r") 'jupyter-repl-restart-kernel)

(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-c") (acg/jupyter-eval-with 'acg/mark-dwim))
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-p") (acg/jupyter-eval-with 'mark-page))
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-l") 'acg/jupyter-eval-region-lines)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-d") 'acg/jupyter-send-defun-body)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-e") 'acg/jupyter-open-python-variable-external-app)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-o") nil)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-o t") 'acg/jupyter-toggle-use-overlays-repl)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-o r") 'jupyter-eval-remove-overlays)

