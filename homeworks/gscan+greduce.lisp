(defun gscan (fn glist &optional (val nil))
  (if (gnull glist)
      nil
      (gcons (funcall fn val (gcar glist))
	     (gscan fn (gcdr glist) (funcall fn val (gcar glist))))))
