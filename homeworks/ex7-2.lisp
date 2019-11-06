(defun map-file (function pathname)
  (with-open-file (in pathname)
    (do ((exp (read in nil 'the-end) (read in nil 'the-end)))
	((eql exp 'the-end))
      (funcall function exp))))

(defun map-stream (function stream)
  (do ((exp (read stream nil 'the-end-my-life) (read stream nil 'the-end-my-life)))
      ((eql exp 'the-end-my-life))
    (funcall function exp)))

