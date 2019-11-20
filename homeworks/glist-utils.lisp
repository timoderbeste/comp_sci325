(defun gnth (n glist)
  (if (= n 0)
      (gcar glist)
      (gnth (1- n) (gcdr glist))))

(defun gmap (function glist)
  (if (gnull glist)
      nil
      (gcons (funcall function (gcar glist))
	     (gmap function (gcdr glist)))))



;;; I think pred is a better variable name here?
(defun gfilter (pred glist)
  (cond ((gnull glist) nil)
	((funcall pred (gcar glist))
	 (gcons (gcar glist) (gfilter pred (gcdr glist))))
	(t (gfilter pred (gcdr glist)))))
