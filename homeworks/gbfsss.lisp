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
	    (bfs (append (cdr paths) (gen-new-paths path next-states)) pred gen)))))

(defun shortest-path (start end net)
  (reverse (bfs (list (list start))
                (lambda (state) (eql state end))
				(lambda (path) (assoc (car path) net)))))
