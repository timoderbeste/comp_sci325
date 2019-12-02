;;;(defun make-best-change-helper (sum coin-types comb-to-count))

(defun make-best-change (sum &optional (coin-types '(25 10 5 1)))
  (do ((rest-coin-types coin-types (cdr rest-coin-types))
       (curr-best-comb nil
		       (let ((curr-coin-type (car rest-coin-types)))
			 (if (> curr-coin-type sum)
			     curr-best-comb
			     (let ((curr-comb (cons curr-coin-type
						    (make-best-change (- sum curr-coin-type) coin-types))))
			       ;;;(print curr-comb)
			       (if (or (null curr-best-comb) (> (length curr-best-comb) (length curr-comb)))
				   curr-comb
				   curr-best-comb))))))
      ((null rest-coin-types) curr-best-comb)))
