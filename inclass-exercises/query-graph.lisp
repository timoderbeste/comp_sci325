(defparameter *facts*
  '((john knows karen)
    (karen knows alex)
    (karen name "karen")
    (alex name "alex")))

(defun var-p (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

(defun update-blists (var item blists)
  (mapcan (lambda (blist)
	    (let ((new-blist (update-blists var item blist)))
	      (and new-blist (list new-blist))))
	  blists))

(defun match-part (x y blists)
  (cond ((eql x y) blists)
	((not (var-p x)) nil)
	(t (update-blists x y blists))))

(defun match (query fact &optional (blists '(())))
  (cond ((null query) blists)
	(t
	 (match (cdr query) (cdr fact)
		(match-part (car query) (car fact) blists)))))

(defun query (triple)
  (remove-if-not (lambda (fact) (match triple fact))
		 *facts*))


(defun query-triple (query blists)
  (mapcan (lambda (fact) (match query fact blists))
       *facts*))

(defun query-graph (triples &optional (blists '(())))
  (cond ((null triples) blists)
	((null blists) nil)
	(t (query-graph (cdr triples)
			(query-triple (car triples) blists)))))
