;;; I am checking if rest is gnull. But even without it it still seems to work. 
(defun gnth (n glist)
  (do ((i 0 (1+ i))
       (rest glist (gcdr rest)))
      ((or (= i n) (gnull rest)) (gcar rest))))

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
