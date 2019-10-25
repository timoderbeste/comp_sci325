(setf graph-wo-cycle '((a b c) (b c) (c d)))
(setf graph-w-cycle '((a b c) (b c) (c a) (d c)))

(defun new-paths-old (path node net end)
  (cond ((member end (cdr (assoc node net)))
	 (list* t end path))
	(t (cons nil (mapcar #' (lambda (n)
				  (cons n path))
				(remove-if (lambda (x) (member x path)) (cdr (assoc node net))))))))

(defun new-paths (path node net)
  (mapcar (lambda (n)
	    (unless (member n path)
	      (cons n path)))
	  (cdr (assoc node net))))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (member end (assoc node net))
	      (reverse (cons end path))
	      (let ((new-path (new-paths path node net)))
		(bfs end
		     (append (cdr queue) new-path) net)))))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))
