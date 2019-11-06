(defun gen-new-paths (path gen)
  (mapcan (lambda (s)
	    (if (member s path)
		nil
		(list (cons s path))))
	  (cdr (funcall gen path))))

(defun find-goal (pred gen path)
  (let ((next-states (funcall gen path)))
    (and next-states
	 (find-if (lambda (next-state) (funcall pred next-state)) next-states))))

(defun bfs (paths pred gen)
  (if (null paths)
      nil
      (let* ((path (car paths))
	     (found-goal (find-goal pred gen path)))
	(if found-goal
	    (cons found-goal path)
	    (let ((new-paths (gen-new-paths path gen)))
	      (bfs (append (cdr paths) new-paths) pred gen))))))

(defun shortest-path (start end net)
  (let* ((pred (lambda (state)
		(eql state end)))
	 (gen (lambda (path)
		(assoc (car path) net)))
	 (path (bfs (list (list start)) pred gen)))
    (reverse  path)))
