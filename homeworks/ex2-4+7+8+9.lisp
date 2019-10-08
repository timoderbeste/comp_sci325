;;; ex2-4
(defun greater (n1 n2)
  (if (> n1 n2)
      n1
      n2))

;;; ex2-7
(defun has-list-p (lst)
  (cond ((null lst)
	 nil)
	((listp (car lst))
	 t)
	((has-list-p (cdr lst))
	 t)
	(t
	 nil)))	

;;; ex 2-8
(defun print-dots-recursive (n)
  (unless (= n 0)
    (format t ".")
    (print-dots-recursive (1- n))))

(defun get-a-count-recursive (lst)
  (cond ((null lst) 0)
	((eql 'a (car lst)) (1+ (get-a-count-recursive (cdr lst))))
	(t (get-a-count-recursive (cdr lst)))))

(defun print-dots-iterative (n)
  (do ((i 1 (1+ i)))
      ((> i n) nil)
    (format t ".")))

(defun get-a-count (lst)
  (loop for obj in lst count (equalp 'a obj)))

;;; ex 2-9
(defun summit (lst)
  (apply #'+ (remove nil lst)))

(defun summit (lst)
  (cond ((null lst) 0)
	((null (car lst)) (summit (cdr lst)))
	(t (+ (car lst) (summit (cdr lst))))))
