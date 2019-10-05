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
(defun print-dots-recursive (n)
  (unless (= n 0)
	 (format t ".")
	 (print-dots-recursive (1- N))))

(defun get-a-count-recursive (lst)
  (cond ((not lst) 0)
	((eql 'a (car lst)) (1+ (get-a-count-recursive (cdr lst))))
	(t (get-a-count-recursive (cdr lst)))))

(defun print-dots-iterative (n)
  (do ((i 1 (1+ i)))
      ((> i n) nil)
    (format t ".")))

(defun get-a-count-iterative (lst)
  (let ((num 0))
    (loop for obj in lst
	 do (if (eql 'a obj)
	     (incf num)
	     nil))
    num))

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
