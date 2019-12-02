;;;(defun make-best-change-helper (sum coin-types comb-to-count))

(defun make-best-change-mem (sum coin-types prev-comb comb-to-remain)
  (let ((remain
	 (progn (unless (cdr (assoc prev-comb comb-to-remain :test equal))
		  (setf comb-to-remain (cons (cons prev-comb (- sum (apply #'+ prev-comb))) comb-to-remain)))
		(cdr (assoc prev-comb comb-to-remain :test equal)))))
    (do ((rest-coin-types coin-types (cdr rest-coin-types))
	 (

(defun make-best-change-helper (sum coin-types)
  (do ((rest-coin-types coin-types (cdr rest-coin-types))
       (curr-best-comb nil
		       (let ((curr-coin-type (car rest-coin-types)))
			 (if (> curr-coin-type sum)
			     curr-best-comb
			     (let ((curr-comb (cons curr-coin-type
						    (make-best-change-helper (- sum curr-coin-type) coin-types))))
			       (if (or (null curr-best-comb)
				       (or (< (apply #'+ curr-best-comb) (apply #'+ curr-comb))
					   (> (length curr-best-comb) (length curr-comb))))
				   curr-comb
				   curr-best-comb))))))
      ((null rest-coin-types) curr-best-comb)))

(defun count-coins (best-comb coin-types)
  (do ((rest-coin-types coin-types (cdr rest-coin-types))
       (type-to-count nil (cons (cons (car rest-coin-types) 0) type-to-count)))
      ((null rest-coin-types)
       (do ((rest-comb best-comb (cdr rest-comb)))
	   ((null rest-comb)
	    (do ((rest-type-to-count type-to-count (cdr rest-type-to-count))
		 (counts nil (cons (cdar rest-type-to-count) counts)))
		((null rest-type-to-count) (values-list counts))))
	 (setf (cdr (assoc (car rest-comb) type-to-count)) (1+ (cdr (assoc (car rest-comb) type-to-count))))))))


(defun make-best-change (sum &optional (coin-types '(25 10 5 1)))
  (count-coins (make-best-change-helper sum coin-types) coin-types))
