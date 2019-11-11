(defun gen-new-paths (path next-states)
  (mapcan (lambda (s)
	    (if (member s path)
		nil
		(list (cons s path))))
	  (cdr next-states)))

(defun bfs (paths pred gen)
  (if (null paths)
      nil
      (let* ((path (car paths))
	     (next-states (funcall gen path))
	     (found-goal (find-if pred next-states)))
	(if found-goal
	    (cons found-goal path)
	    (let ((new-paths (gen-new-paths path next-states)))
	      (bfs (append (cdr paths) new-paths) pred gen))))))

(defun shortest-path (start end net)
  (let* ((pred (lambda (state)
		(eql state end)))
	 (gen (lambda (path)
		(assoc (car path) net)))
	 (path (bfs (list (list start)) pred gen)))
    (reverse  path)))
