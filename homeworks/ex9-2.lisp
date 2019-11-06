(defun make-change-helper (total coin-types)
  (and coin-types
       (let ((coin-type (car coin-types)))
	 (multiple-value-bind (num-coins remains) (floor total coin-type)
	   (cons num-coins (make-change-helper remains (cdr coin-types)))))))

(defun make-change (total &optional (coin-types '(25 10 5 1)))
  (values-list (make-change-helper total coin-types)))
