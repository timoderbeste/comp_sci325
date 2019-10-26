(defun map-file (function pathname))

(defun map-stream (function stream)
  (let ((exps nil))
    (do ((exp (read stream nil) (read stream nil)))
	((null exp) (length exps))
      (print exp)
      (push exp exps))
    (nreverse (mapcar function exps))))
