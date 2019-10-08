(defun show-dots-helper (lst)
  (cond ((and lst (atom lst))
	 (write-to-string lst))
	((not (null lst))
	 (concatenate 'string "(" (show-dots-helper (car lst)) " . " (show-dots-helper (cdr lst)) ")" ))
	(t "NIL")
	))

(defun show-dots (lst)
  (format t (show-dots-helper lst)))

(defun show-list-content (lst)
  (cond ((and (null (cdr lst)) (atom (car lst)))
	 (write-to-string (car lst)))
	((and (null (cdr lst)) (consp (car lst)))
	 (concatenate 'string "[" (show-list-content (car lst)) "]"))
	((and (atom (cdr lst)) (atom (car lst)))
	 (concatenate 'string (write-to-string (car lst)) " . " (write-to-string (cdr lst))))
	((and (atom (cdr lst)) (consp (car lst)))
	 (concatenate 'string "[" (show-list-content (car lst)) "]" " . " (write-to-string (cdr lst))))	
	((atom (car lst))
	 (concatenate 'string (write-to-string (car lst)) " " (show-list-content (cdr lst))))
	((consp (car lst))
	 (concatenate 'string "[" (show-list-content (car lst)) "]" " " (show-list-content (cdr lst))))
	(t "")))

(defun show-list (lst)
  (if (atom lst)
      (format t (write-to-string lst))
      (format t (concatenate 'string "[" (show-list-content lst) "]"))))




      ;;;(format t (concatenate 'string "[" (show-list-content lst) "]"))))
