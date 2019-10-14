(defun isa (x y)
  (or (eql x y)
      (some (lambda (fact)
	      (and (eql (car fact) x)
		   (eql (cadr fact) 'isa)
		   (isa (caddr fact) y)))
	    *facts*)))

(defun retrieve (x y)
  (remove-if-not (lambda (fact)
		   (and (eql y (caddr fact))
			(isa x (car fact))))
		 *facts*))

(defun more-specific-p (fact1 fact2)
  (and (not (eql (car fact1) (car fact2)))
       (isa (car fact1) (car fact2))))


(defun prune (facts)
  (remove-if (lambda (fact1)
	       (some (lambda (fact2)
		       (more-specific-p fact2 fact1))
		     facts))
	     facts))