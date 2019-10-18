(defun map-range (func start end)
  (do ((i start (+ i (if (< start end) 1 -1)))
       (results nil results))
      ((= end i) (reverse results))
    (push (funcall func i) results)))

(defun find-range (pred start end)
  (do ((i start (+ i (if (< start end) 1 -1)))
       (result nil (if (funcall pred i)
		       i
		       nil)))
      ((or (= end i) (not (null result))) result)))

(defun every-range (pred start end)
  (do ((i start (+ i (if (< start end) 1 -1)))
       (result t (and result (funcall pred i))))
      ((or (= end i) (null result)) result)))

(defun reduce-range (func start end &optional (init nil))
  (do ((i start (+ i (if (< start end) 1 -1)))
       (result init (funcall func result i)))
      ((= end i) result)))
