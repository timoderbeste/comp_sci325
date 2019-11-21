;;; can we use recursion or do we have to use iteration? 
(defun gscan (fn glist &optional (val nil))
  (if (gnull glist)
      nil
      (gcons (funcall fn val (gcar glist))
	     (gscan fn (gcdr glist) (funcall fn val (gcar glist))))))

(defun greduce-helper (fn glist curr start end acc)
  (cond ((and end (= curr end)) acc)
	((gnull glist) acc)
	((< curr start)
	 (greduce-helper fn (gcdr glist) (1+ curr) start end acc))
	(t
	 (greduce-helper fn (gcdr glist) (1+ curr) start end (funcall fn acc (gcar glist))))))
	

(defun greduce (fn glist &key (start 0) (end nil) (initial-value nil))
  (greduce-helper fn glist 0 start end initial-value))
