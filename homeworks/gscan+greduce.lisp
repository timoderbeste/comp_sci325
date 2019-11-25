;;; can we use recursion or do we have to use iteration? 
(defun gscan (fn glist &optional (val nil))
  (if (gnull glist)
      nil
      (gcons (funcall fn val (gcar glist))
	     (gscan fn (gcdr glist) (funcall fn val (gcar glist))))))

(defun greduce (fn glist &key (start 0) (end nil) (initial-value nil))
  (if (gnull glist)
      initial-value
      (do* ((curr-index 0 (1+ curr-index))
	    (rest glist (gcdr rest))
	    (acc (if (= start curr-index)
		     (funcall fn initial-value (gcar rest))
		     initial-value)
		 (if (>= curr-index start)
		     (funcall fn acc (gcar rest))
		     acc)))
	   ((or (and end (= curr-index (1- end))) (and (not end) (gnull (gcdr rest)))) acc))))
