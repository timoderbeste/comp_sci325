(defun show-dots-old (lst)
  (format t (show-dots-helper lst)))

(defun show-dots-helper-old (lst)
  (cond ((null lst) "NIL")
	((consp (car lst))
	 (concatenate 'string (show-dots-helper (car lst)) (show-dots-helper (cdr lst)))
	 )
	(t
	 (concatenate 'string (concatenate 'string (format nil "(~A . " (car lst)) (show-dots-helper (cdr lst))) ")")
	 )
	))

(defun show-dots (lst)
  (cond ((null lst) (format t "NIL"))
	((consp (car lst)) (show-dots-nested (car lst)) (show-dots (cdr lst)))
	(t (format t "(~A . " (car lst)) (show-dots (cdr lst)) (format t ")"))
	))

(defun show-dots-nested (lst)
  (cond ((null lst) (format t ""))
	((consp (car lst)) (show-dots-nested (car lst)) (show-dots-nested (cdr lst)))
	(t (format t "(~A . " (car lst)) (show-dots-nested (cdr lst)))
	))
