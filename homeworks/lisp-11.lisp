(defun map-range (func start end)
  (do ((i start (+ i (if (< start end) 1 -1)))
       (results nil (cons (funcall func i) results)))
       ((= end i) (nreverse results))))

(defun find-range (pred start end)
  (do ((i start (+ i (if (< start end) 1 -1))))
      ((or (= end i) (funcall pred i)) (if (= end i)
					   nil
					   i))))

(defun every-range (pred start end)
  (do ((i start (+ i (if (< start end) 1 -1))))
      ((or (= end i) (not (funcall pred i))) (if (= end i)
						 t
						 nil))))

(defun reduce-range (func start end &optional (init nil))
  (do ((i start (+ i (if (< start end) 1 -1)))
       (result init (funcall func result i)))
      ((= end i) result)))
