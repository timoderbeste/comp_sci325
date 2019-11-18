;;; I am mimicking random-choice.. the original mapcar seems to evalue all exprs so I set a condition and only evaluate if i == n-1. not sure if this is "cheating"...
(defmacro nth-expr (n &rest exprs)
  `(case ,n
     ,@(let ((i 0))
	 (mapcar #'(lambda(expr)
		     `(,(incf i) (if `(= i n) ,expr nil)))
		  exprs))))


(defmacro n-of (n expr)
  (let ((g-n (gensym))
	(g-i (gensym))
	(g-lst (gensym)))
    `(do ((,g-i 0 (1+ ,g-i))
	  (,g-n ,n)
	  (,g-lst nil (cons ,expr ,g-lst)))
	 ((= ,g-i ,g-n) (nreverse ,g-lst)))))
