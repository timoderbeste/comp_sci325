;;; can we use recursion or do we have to use iteration? 
(defun gscan (fn glist &optional (val nil))
  (if (gnull glist)
      nil
      (gcons (funcall fn val (gcar glist))
	     (gscan fn (gcdr glist) (funcall fn val (gcar glist))))))

(defun greduce (fn glist &key (start 0) (end nil) (initial-value nil))
  (do ((curr-index 0 (1+ curr-index))
       (rest glist (gcdr rest))
       (acc initial-value (if (>= curr-index start) (funcall fn acc (gcar rest)) acc)))
      ((or (and end (= curr-index end)) (gnull rest)) acc)))
