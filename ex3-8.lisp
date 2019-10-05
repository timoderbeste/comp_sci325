(defun show-dots (lst)
  (format t (show-dots-helper lst)))

(defun show-dots-helper (lst)
  (cond ((and lst (atom lst)) (write-to-string lst))
	((not (null lst)) (concatenate 'string "(" (show-dots-helper (car lst)) " . " (show-dots-helper (cdr lst)) ")" ))
	(t "NIL")
	))
