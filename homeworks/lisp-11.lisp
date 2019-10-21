(defun map-range (func start end)
  (do ((update (if (< start end) 1 -1) update)
       (i start (+ i update))
       (results nil (cons (funcall func i) results)))
      ((= end i) (nreverse results))))

(defun find-range (pred start end)
  (do ((update (if (< start end) 1 -1) update)
       (i start (+ i update)))
      ((or (= end i) (funcall pred i))
       (if (= end i) nil i))))

(defun every-range (pred start end)
  (do ((update (if (< start end) 1 -1) update)
       (i start (+ i update)))
      ((or (= end i) (not (funcall pred i))) (= end i))))

(defun reduce-range (func start end &optional (init nil))
  (do ((update (if (< start end) 1 -1) update)
       (i start (+ i update))
       (result init (funcall func result i)))
      ((= end i) result)))
