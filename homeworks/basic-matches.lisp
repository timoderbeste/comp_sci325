(in-package #:exmatch)

(defun ?or (xs y lsts)
  (cond ((null lsts) nil)
	((null xs) nil)
	(t (append (match-p (car xs) y lsts)
		   (?or (cdr xs) y lsts)))))

(defun ?not (x y lsts)
  (cond ((null lsts) nil)
	((null x) nil)
	(t (append (if (match-p (car x) y (list (car lsts)))
		       nil
		       (list (car lsts)))
		   (?not x y (cdr lsts))))))

(defun ?= (xs y lsts)
  (let ((sub-pattern (car xs))
	(function-name (cadr xs))
	(args (cddr xs)))
    (match-p sub-pattern (apply function-name (cons y args)) lsts)))
