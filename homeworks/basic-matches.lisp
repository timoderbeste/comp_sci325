(in-package #:exmatch)

(defun ?or (xs y lsts)
  (cond ((null lsts) nil)
	((null xs) nil)
	(t (append (match-p (car xs) y lsts)
		   (?or (cdr xs) y lsts)))))

;;; Not sure if this counts as mouse tail? I am trying to make it clear the parts for functional calls.

(defun ?not (x y lsts)
  (cond ((null lsts) nil)
	((null x) nil)
	(t (remove-if #'(lambda (lst)
			  (match-p (car x) y (list lst)))
		      lsts))))

(defun ?= (xs y lsts)
  (destructuring-bind (sub-pattern function-name . args) xs
    (match-p sub-pattern (apply function-name (cons y args)) lsts)))    
