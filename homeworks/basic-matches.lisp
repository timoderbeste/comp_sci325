(in-package #:exmatch)

(defun ?or (xs y lsts)
  (cond ((null lsts)
	 nil)
	 ((null xs)
	 nil)
	 (t (append (match-p (car xs) y lsts)
		    (?or (cdr xs) y lsts)))))

(defun ?not (xs y lsts))

(defun ?= (xs y lsts))
