(setf graph-wo-cycle '((a b c) (b c) (c d)))
(setf graph-w-cycle '((a b c) (b c) (c a) (d c)))

(defun new-paths (path node net end)
  (if (member end (cdr (assoc node net)))
      (throw 'abort (reverse (cons end path)))
      (mapcar #' (lambda (n)
		   (cons n path))
		 (remove-if (lambda (x) (member x path)) (cdr (assoc node net))))))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (reverse path)
	      (bfs end
		   (append (cdr queue)
			   (new-paths path node net end))
		   net))))))

(defun shortest-path (start end net)
  (catch 'abort
    (bfs end (list (list start)) net)))
