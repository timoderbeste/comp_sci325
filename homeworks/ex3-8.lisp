(defun show-dots (lst)
  (cond ((atom lst)
         (format t "~S" lst))
	(t
	 (format t "(")
	 (show-dots (car lst))
	 (format t " . ")
	 (show-dots (cdr lst))
	 (format t ")"))))

(defun show-list-proper (lst)
  (cond ((atom (car lst))
	 (format t "~S" (car lst)))
	(t
	 (format t "[")
	 (show-list-full (car lst))
	 (format t "]"))))

(defun show-list-improper (lst)
  (cond ((atom (car lst))
	 (format t "~S" (car lst))
	 (format t " . ")
	 (format t "~S" (cdr lst)))
	(t
	 (format t "[")
	 (show-list-full (car lst))
	 (format t "]")
	 (format t " . ")
	 (format t "~S" (cdr lst)))))

(defun show-list-rest (lst)
  (cond ((atom (car lst))
	 (format t "~S " (car lst))
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
	 (format t "~S" lst))
	(t
	 (format t "[")
	 (show-list-full lst)
	 (format t "]"))))
