(require 'jupyter)
(require 'expand-region)

;; Copy evaluation input to a REPL cell
(setq jupyter-repl-echo-eval-p t)

;; Custom faces
(set-face-attribute 'jupyter-repl-traceback nil :background "#660000")


;; Custom functions
;;
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
     (region-beginning)
     (region-end))))

(defun acg/jupyter-send (args)
  "Send text to Jupyter REPL."
  (interactive "P")
  )

(defun acg/jupyter-eval-paragraph-or-region (arg)
  "Calls `acg/jupyter-eval-region-lines' on current paragraph."
  (interactive "P")
  (save-mark-and-excursion
    (unless (use-region-p)
      (er/mark-paragraph))
    (acg/jupyter-eval-region-lines)))

(defun acg/jupyter-eval-defun (arg)
  "Calls `jupyter-eval-region' on current paragraph."
  (interactive "P")
  (save-mark-and-excursion
    (er/mark-defun)
    (acg/jupyter-eval-region-lines)))

(defun acg/jupyter-eval-page (arg)
  "Calls `jupyter-eval-region' on current page."
  (interactive "P")
  (save-mark-and-excursion
    (mark-page)
    (acg/jupyter-eval-region-lines)))

(defun acg/jupyter-send-defun-body (arg)
  "aojaoiajoa"
  (interactive "P")
  (save-mark-and-excursion
    (er/mark-defun)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (forward-line)
    (acg/jupyter-eval-region-lines)))

(defun acg/jupyter-eval-dwim (arg)
  "My custom jupyter eval."
  (interactive "P")
  (save-mark-and-excursion
    (unless (use-region-p)
      (er/mark-paragraph)
      (if (acg/region-inside-defun-p)
          (er/mark-defun)))
    (acg/jupyter-eval-region-lines)))

;; @todo: eval up to point if universal argument is supplied
;; @todo: send text to buffer if universal argument is supplied

;; Keybindings
(define-key python-mode-map (kbd "C-c r") 'jupyter-run-repl)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-c") 'acg/jupyter-eval-dwim)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-p") 'acg/jupyter-eval-page)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-l") 'acg/jupyter-eval-region-lines)
(define-key jupyter-repl-interaction-mode-map (kbd "C-c C-d") 'acg/jupyter-send-defun-body)
