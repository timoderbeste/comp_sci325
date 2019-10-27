(defun gen-new-paths (path gen)
  (mapcan (lambda (s)
	    (unless (member s path)
	      (list (cons s path))))
	  (cdr (funcall gen path))))

(defun bfs (paths pred gen)
  (if (null paths)
      nil
      (let* ((path (car paths))
	     (state (car path)))
	(if (funcall pred state)
	    path
	    (let ((new-paths (gen-new-paths path gen)))
	      (bfs (append (cdr paths) new-paths) pred gen))))))

(defun shortest-path (start end net)
  (let* ((pred (lambda (state)
		(member end (assoc state net))))
	 (gen (lambda (path)
		(assoc (car path) net)))
	 (path (bfs (list (list start)) pred gen)))
    (if (null path)
	nil
	(reverse (cons end path)))))
