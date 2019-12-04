(defun count-coins (best-comb coin-types)
  (do ((rest-coin-types coin-types (cdr rest-coin-types))
       (type-to-count nil (acons (car rest-coin-types) 0 type-to-count)))
      ((null rest-coin-types)
       (do ((rest-comb best-comb (cdr rest-comb)))
	   ((null rest-comb)
	    (do ((rest-type-to-count type-to-count (cdr rest-type-to-count))
		 (counts nil (cons (cdar rest-type-to-count) counts)))
		((null rest-type-to-count) (values-list counts))))
	 (incf (cdr (assoc (car rest-comb) type-to-count)))))))


(defun make-best-change (sum &optional (coin-types '(25 10 5 1)))
  (count-coins
   (let ((all-combs nil))
     (funcall (defun helper (sum coin-types prev-comb)
	      (do ((rest-coin-types coin-types (cdr rest-coin-types))
		   (curr-best-comb
		    nil
		    (let ((curr-coin-type (car rest-coin-types)))
		      (cond ((or (> curr-coin-type sum)
				(member (cons curr-coin-type prev-comb) all-combs :test #'equal))
			     curr-best-comb)
			    (t
			     (let ((curr-comb
				    (cons curr-coin-type
					 (helper (- sum curr-coin-type)
						 coin-types
						 (cons curr-coin-type prev-comb)))))
			       (if (or (null curr-best-comb)
				       (> (apply #'+ curr-comb) (apply #'+ curr-best-comb))
				      (and (= (apply #'+ curr-comb) (apply #'+ curr-best-comb))
					   (< (length curr-comb) (length curr-best-comb))))
				   curr-comb
				   curr-best-comb)))))))
		  ((null rest-coin-types) curr-best-comb))) sum coin-types nil)) coin-types))
