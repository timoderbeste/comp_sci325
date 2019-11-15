(defun map-stream (function stream)
  (do* ((g (gensym) g)
	(exp (read stream nil g) (read stream nil g)))
       ((eql exp g))
    (funcall function exp)))

(defun map-file (function pathname)
  (with-open-file (in pathname)
    (map-stream function in)))
