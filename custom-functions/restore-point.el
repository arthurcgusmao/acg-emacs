;;; restore-point.el --- Restore point position after mark or scroll functions

;; Copyright (C) 2019 Arthur Colombini Gusmão

;; Author: Arthur Colombini Gusmão
;; Keywords: point, mark, scroll, mouse-wheel, restore

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; restore-point-mode is a global minor mode.

;; When the minor mode is active and you execute `keyboard-quit' (usually bound
;; to C-g or ESC) after a sequence of mark or scroll commands (or any other
;; predefined set of commands of your choice, configurable in
;; `rp/restore-point-commands'), the point (cursor) position is restored to its
;; previous location.

;; This turns out to be especially helpful when scroll commands moved the point
;; from where you had it in the first place, or if you mistakenly executed some
;; command (such as mark-whole-buffer) that moved the point position and you
;; want to restore it. Or, yet, if there is any command that moves the point
;; position that you want to be able to easily restore.

;; The core idea of this package was firstly conceived and implemented by the
;; author alone as a custom function; however, the code herein was further
;; improved and refactored as a package by adapting code and ideas from
;; `smart-mark.el', a very similar package with similar purpose developed by
;; Kai Yu, which can be found in
;; https://github.com/zhangkaiyulw/smart-mark/blob/master/smart-mark.el

;; Code:

(defvar-local rp/point-ring nil "List of former point
positions of the current buffer, most recent first.")
(put 'rp/point-ring 'permanent-local t)

(defvar rp/point-ring-max 128 "Maximum size of point ring.
Start discarding off end if gets this big.")

(defvar rp/restore-point-commands
  '(beginning-of-buffer
    end-of-buffer
    mark-defun
    mark-page
    mark-paragraph
    mark-sexp
    mark-whole-buffer
    mark-work
    mwheel-scroll
    scroll-bar-drag
    scroll-bar-scroll-down
    scroll-bar-scroll-up
    scroll-bar-toolkit-scroll
    scroll-down-command
    scroll-other-window
    scroll-other-window-down
    scroll-up scroll-down
    scroll-up-command)
  "List of commands for which the point position will be pushed
to `rp/point-ring' before being called.")

(defun rp/push-point-ring ()
  "Push current point position to point ring."
  (interactive)
  ;; Add current point position to ring
  (setq rp/point-ring (cons (copy-marker (point-marker)) rp/point-ring))
  ;; Discard oldest elements if ring limit has been exceeded
  (when (> (length rp/point-ring) rp/point-ring-max)
    (move-marker (car (nthcdr rp/point-ring-max rp/point-ring)) nil)
    (setcdr (nthcdr (1- rp/point-ring-max) rp/point-ring) nil)))

(defun rp/restore-point-position ()
  "Restore most recent point position from ring."
  (interactive)
  (goto-char (nth 0 rp/point-ring)))

;; Pre-command hook function
(defun rp/pre-command ()
  "Push point position to ring when transitioning to a
restore-point command from an ordinary one. To be added to
`pre-command-hook' list when activating this minor mode."
  (when (and (not (eq this-command last-command))
             (memq this-command rp/restore-point-commands)
             (not (memq last-command rp/restore-point-commands)))
    (rp/push-point-ring)))

;; Restore point advice function
(defun rp/cond-restore-point (&rest args)
  "Restore point position if last command in
`rp/restore-point-commands' list. To be added as advice to
`keyboard-quit' when activating this minor mode."
  (when (memq last-command rp/restore-point-commands)
    (rp/restore-point-position)))


;;;###autoload
(define-minor-mode restore-point-mode
  "Mode for restoring point position w/ `keyboard-quit' after
predefined (usually scroll or mark) commands are executed."
  :global t
  (if restore-point-mode
      (progn
        (add-hook 'pre-command-hook 'rp/pre-command)
        (advice-add #'keyboard-quit
                    :before #'rp/cond-restore-point))
    (progn
      (remove-hook 'pre-command-hook 'rp/pre-command)
      (advice-remove 'keyboard-quit #'rp/cond-restore-point))))

(provide 'restore-point)
;;; restore-point.el ends here
