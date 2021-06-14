;; String manipulation library
;; Ref: https://github.com/magnars/s.el
(use-package s
  :init
  (defun acg/s-spaced-words (s)
    "Returns a string w/ words split by space. Similar to
`s-dashed-words', but with spaces instead of dashes."
    (s-join " " (s-split-words s)))

  (defun acg/toggle-word-separator ()
    "Prompts the user with alternative case options for the
symbol at point or the current text selection."
    (interactive)
    (let (beg end old-s new-s)
      ;; Get region limits to operate upon
      (cond
       ((eq last-command this-command)
        (setq beg tmp/beg
              end tmp/end))
       ((use-region-p)
        (setq beg (region-beginning)
              end (region-end)))
       (t
        (save-mark-and-excursion
          (skip-chars-backward "[:alnum:]-_") (setq beg (point))
          (skip-chars-forward "[:alnum:]-_") (setq end (point)))))
      ;; Store string to be manipulated
      (setq old-s (buffer-substring beg end))
      ;; Replace content in region
      (replace-region-contents
       beg end
       (lambda ()
         (setq new-s
               (cond
                ((s-equals? old-s (s-dashed-words old-s))
                 (s-snake-case old-s))
                ((s-equals? old-s (s-snake-case old-s))
                 (s-downcase (acg/s-spaced-words old-s)))
                ((s-equals? old-s (acg/s-spaced-words old-s))
                 (s-lower-camel-case old-s))
                (t
                 (s-dashed-words old-s))))))
      ;; Remember region in case command is re-invokated
      (setq tmp/beg beg
            tmp/end (+ end (- (length new-s) (length old-s))))))

  :bind
  (("M-C" . acg/toggle-word-separator)))


(defun append-or-remove-char-to-eol (char-code char)
  "Add/removes specific char at the end of the line and return to current position"
  (setq current (point))
  (end-of-line)
  (if (not (= (preceding-char) char-code))
    (insert char)
    (delete-backward-char 1))
  (goto-char current))

(defun append-or-remove-semicolon-to-eol ()
  "Add/removes semicolon to the end of the line"
  (interactive)
  (append-or-remove-char-to-eol 59 ";"))

(defun append-or-remove-comma-to-eol ()
  "Add/removes comma to the end of the line"
  (interactive)
  (append-or-remove-char-to-eol 44 ","))

(global-set-key (kbd "C-;") 'append-or-remove-semicolon-to-eol)
(global-set-key (kbd "C-,") 'append-or-remove-comma-to-eol)



(defun acg/toggle-word-case ()
  "Toggle case of the current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2017-04-19"
  (interactive)
  (let (
        (deactivate-mark nil)
        -p1 -p2)
    (if (use-region-p)
        (setq -p1 (region-beginning)
              -p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]-_")
        (setq -p1 (point))
        (skip-chars-forward "[:alnum:]-_")
        (setq -p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region -p1 -p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region -p1 -p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region -p1 -p2)
      (put this-command 'state 0)))))

(global-set-key (kbd "M-c") 'acg/toggle-word-case)



(defun acg/split-region-contents-into-lines ()
  "Split the region contents into separate lines, using a
separator regexp, for which the user is prompted. If region is
inactive, operates on the current line."
  (interactive)
  (when (not (use-region-p))
    (acg/select-current-line))
  (let ((REGION-STRING (buffer-substring-no-properties
                        (region-beginning) (region-end)))
        (SEPARATORS (read-string "Enter regexp separators (leave blank for default \"[ \\f\\t\\n\\r\\v]+\"): "
                                 nil nil split-string-default-separators)))
    (delete-region (region-beginning) (region-end))
    (insert
     (string-join
      (split-string REGION-STRING SEPARATORS)
      "\n"))))


(defun acg/flush-empty-lines ()
  "Runs `flush-lines' and pre-fill the prompt with a set of chars
that correspond to whitespace or newlines."
  (interactive)
  (flush-lines "^[ \\f\\t\\n\\r\\v	]*[]?$"))
