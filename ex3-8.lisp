(defun show-dots-helper (lst)
  (cond ((and lst (atom lst)) (write-to-string lst))
	((not (null lst)) (concatenate 'string "(" (show-dots-helper (car lst)) " . " (show-dots-helper (cdr lst)) ")" ))
	(t "NIL")
	))

(defun show-dots (lst)
  (format t (show-dots-helper lst)))

(defun proper-list? (lst)
  (or (null lst)
      (and (consp lst)
	   (proper-list? (cdr lst)))))


(defun show-list-content (lst)
  (cond ((and (atom (car lst)) (null (cdr lst)))
	 (write-to-string (car lst)))
	((and (atom (car lst)) (not (null (cdr lst))))
	 (concatenate 'string (write-to-string (car lst)) " " (show-list-content (cdr lst))))
	((and (not (atom (car lst))) (null (cdr lst)))
	 (concatenate 'string "[" (show-list-content (car lst)) "]"))
	((and (not (atom (car lst))) (not (null (cdr lst))))
	 (concatenate 'string "[" (show-list-content (car lst)) "]" " " (show-list-content (cdr lst))))))

(defun show-list (lst)
  (if (atom lst)
      (format t (write-to-string lst))
      (format t (concatenate 'string "[" (show-list-content lst) "]"))))      

