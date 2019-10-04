;;; ex2-4
(defun greater (n1 n2)
  (if (> n1 n2)
      n1
      n2))

;;; ex2-7
(defun has-list-p (lst)
  (if (not lst)
      nil
      (or (listp (car lst))
	  (has-list-p (cdr lst)))))

;;; ex 2-8
(defun print-dots (n)
  (unless (= n 0)
	 (format t ".")
	 (print-dots (1- N))))

(defun get-a-count-old (lst)
  (if (not lst)
      0
      (if (eql 'a (car lst))
	  (1+ (get-a-count (cdr lst)))
	  (get-a-count (cdr lst))
	  )))

(defun get-a-count (lst)
  (cond ((not lst) 0)
	((eql 'a (car lst)) (1+ (get-a-count (cdr lst))))
	(t (get-a-count (cdr lst)))))

;;; ex 2-9
(defun summit-old (lst)
  (if (not lst)
      0
      (if (null (car lst))
	  (summit (cdr lst))
	  (+ (car lst) (summit (cdr lst))))))

(defun summit (lst)
  (cond ((not lst) 0)
	((null (car lst)) (summit (cdr lst)))
	(t (+ (car lst) (summit (cdr lst))))))
