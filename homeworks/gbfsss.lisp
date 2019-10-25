(defun bfs (paths pred gen)
  (if (empty-queue-p paths)
      nil
      (let* ((path (car paths))
	     (state (car path)))
	(if (funcall pred state)
	    path
	    (let ((new-path (funcall gen path)))
	      (bfs (append (cdr paths) new-path) pred gen))))))

(defun shortest-path (start end net)
  (let* ((pred (lambda (state)
		(member end (assoc state net))))
	(gen (lambda (path)
	       (let ((node (car path)))
		 (mapcar (lambda (n)
			   (unless (member n path)
			     (cons n path)))
			 (cdr (assoc node net))))))
	 (path (bfs (list (list start)) pred gen)))
    (if (null path)
	nil
	(reverse (cons end path)))))
