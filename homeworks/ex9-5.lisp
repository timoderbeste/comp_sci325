(defun make-change-helper (total coin-types)  
  (let ((coin-type (car coin-types)))
    (and coin-type
	 (let ((num-coins (floor  total coin-type)))
	   (cons num-coins (make-change-helper (- total (* num-coins coin-type)) (cdr coin-types)))))))

(defun make-change (total &optional (coin-types '(25 10 5 1)))
  (values-list (make-change-helper total coin-types)))
