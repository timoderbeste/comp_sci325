(defun enumerate-coin-nums (sum coin-types comp prev-comb)
  (do ((curr-coin-type (car coin-types))
       (count 0 (1+ count))
       (curr-comb
	nil
	(explore-comb (- sum (* count curr-coin-type))
		      (cdr coin-types)
		      comp
		      (cons count prev-comb))))
      ((< (- sum (* count curr-coin-type)) 0) (funcall comp curr-comb) curr-comb)
	   (when curr-comb
	     (funcall comp curr-comb))))

(defun explore-comb (sum coin-types comp &optional (prev-comb nil))
  (cond ((null coin-types)
	 (list sum (apply #'+ prev-comb) prev-comb))
	((null (cdr coin-types))
	 (multiple-value-bind (f r) (floor sum (car coin-types))
	   (let ((curr-comb (cons f prev-comb)))
	     (list r (apply #'+ curr-comb) curr-comb))))
	(t (enumerate-coin-nums sum coin-types comp prev-comb))))

(defun better-comb-p (curr-comb best-comb)
  (or (null best-comb)
      (< (car curr-comb) (car best-comb))
      (and (= (car curr-comb) (car best-comb))
	   (< (cadr curr-comb) (cadr best-comb)))))

(defun make-best-change (sum &optional (coin-types '(25 10 5 1)))
  (let ((best-comb nil))
    (explore-comb sum
		  coin-types
		  (lambda (curr-comb)
		    (when (better-comb-p curr-comb best-comb)
		      (setf best-comb curr-comb))))
   ;;; (print best-comb)
    (values-list (reverse (caddr best-comb)))))
