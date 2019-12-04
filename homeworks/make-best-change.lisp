(defun make-best-change (sum &optional (coin-types '(25 10 5 1)))
  (values-list (reverse (caddr (make-best-change-helper sum coin-types nil)))))

(defun make-best-change-helper (sum coin-types prev-comb)
  (cond ((null coin-types)
	 (list sum (apply #'+ prev-comb) prev-comb))
	(t 
	 (do ((curr-coin-type (car coin-types))
	      (count 0 (1+ count))
	      (curr-comb nil (make-best-change-helper (- sum (* count curr-coin-type)) (cdr coin-types) (cons count prev-comb)))
	      (curr-best-comb nil (if (or (null curr-best-comb)
					  (< (car curr-comb) (car curr-best-comb))
					  (and (= (car curr-comb) (car curr-best-comb))
					       (< (cadr curr-comb) (cadr curr-best-comb))))
				      curr-comb
				      curr-best-comb)))
	     ((< (- sum (* count curr-coin-type)) 0) (if (or (null curr-best-comb)
							     (< (car curr-comb) (car curr-best-comb))
							     (and (= (car curr-comb) (car curr-best-comb))
								  (< (cadr curr-comb) (cadr curr-best-comb))))
							 curr-comb
							 curr-best-comb))
	   ;;;(format t "curr-comb: ~A curr-best-comb: ~A ~%" curr-comb curr-best-comb)
	   ))))
