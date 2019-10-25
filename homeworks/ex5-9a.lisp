(setf graph-wo-cycle '((a b c) (b c) (c d)))
(setf graph-w-cycle '((a b c) (b c) (c a) (d c)))

(defun new-paths (path node net end)
  (mapcan (lambda (n)
	      (cond ((member end (cdr (assoc node net)))
		     (throw 'abort (reverse (cons end path))))
		    ((not (member n path))
		     (list (cons n path)))
		    (t nil)))
	  (cdr (assoc node net))))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
      (let* ((path (car queue))
	     (node (car path)))
	(bfs end
	     (append (cdr queue)
		     (new-paths path node net end))
	     net))))

(defun shortest-path (start end net)
  (catch 'abort
    (bfs end (list (list start)) net)))
