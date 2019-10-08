(defun handle-atom (lst)
  (numberp lst))

(defun handle-cons (lst)
  (cond ((null lst)
	 nil)
	((atom (car lst))
	 (or (handle-atom (car lst)) (handle-cons (cdr lst))))
	(t
	 (or (handle-cons (car lst)) (handle-cons (cdr lst))))))

(defun has-number-p (lst)
  (if (atom lst)
      (handle-atom lst)
      (handle-cons lst)))
