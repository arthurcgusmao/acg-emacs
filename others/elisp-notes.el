
;; My general notes on elisp


;; Evaluating a list of functions

(defun f1() (message "fjaosdifjpaso"))
(defun f2() (message "blabla"))

(defvar flist nil)
(setq flist nil)

(add-to-list 'flist 'f1 t)
(add-to-list 'flist 'f2 t) ;; last argument t adds to the end of the list

(--each flist (funcall it))

