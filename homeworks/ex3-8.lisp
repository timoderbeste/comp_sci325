(defun show-dots (lst)
  (cond ((and lst (atom lst))
	 (format t "~A" lst))
	((not (null lst))
	 (format t "(")
	 (show-dots (car lst))
	 (format t " . ")
	 (show-dots (cdr lst))
	 (format t ")"))
	(t
	 (format t "NIL"))))

(defun show-list-proper (lst)
  (cond ((atom (car lst))
	 (format t "~A" (car lst)))
	(t
	 (format t "[")
	 (show-list-full (car lst))
	 (format t "]"))))

(defun show-list-improper (lst)
  (cond ((atom (car lst))
	 (format t "~A" (car lst))
	 (format t " . ")
	 (format t "~A" (cdr lst)))
	(t
	 (format t "[")
	 (show-list-full (car lst))
	 (format t "]")
	 (format t " . ")
	 (format t "~A" (cdr lst)))))

(defun show-list-rest (lst)
  (cond ((atom (car lst))
	 (format t "~A " (car lst))
	 (show-list-full (cdr lst)))
	(t
	 (format t "[")
	 (show-list-full (car lst))
	 (format t "] ")
	 (show-list-full (cdr lst)))))

(defun show-list-full (lst)
  (cond ((null (cdr lst))
	 (show-list-proper lst))
	((atom (cdr lst))
	 (show-list-improper lst))
	(t
	 (show-list-rest lst))))

(defun show-list (lst)
  (cond ((atom lst)
	 (format t "~A" lst))
	(t
	 (format t "[")
	 (show-list-full lst)
	 (format t "]"))))
