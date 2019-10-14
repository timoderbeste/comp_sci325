(defparameter *facts*
  '((john knows karen)
    (karen knows alex)
    (karen name "karen")
    (alex name "alex")))

(defun query-helper (rel facts)
  (cond ((null facts)
	 '())
	((eql (cadr (car facts)) rel)
	 (concatenate 'list (cons (car facts) nil) (query-helper rel (cdr facts))))
	(t
	 (query-helper rel (cdr facts)))))
	
(defun query (subj rel obj)
  (query-helper rel *facts*))
