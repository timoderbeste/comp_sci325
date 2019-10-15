(defun map-range-helper (func curr end update)
  (if (eql curr end)
      nil
      (cons (funcall func curr) (map-range-helper func (funcall update curr) end update))))

(defun map-range (func start end)
  (if (< start end)
      (map-range-helper func start end '1+)
      (map-range-helper func start end '1-)))

(defun find-range-helper (pred curr end update)
  (cond ((eql curr end)
	 nil)
	((funcall pred curr)
	 curr)
	(t
	 (find-range-helper pred (funcall update curr) end update))))

(defun find-range (pred start end)
  (if (< start end)
      (find-range-helper pred start end '1+)
      (find-range-helper pred start end '1-)))

(defun every-range-helper (pred curr end update)
  (cond ((eql curr end)
	 t)
	((not (funcall pred curr))
	 nil)
	(t
	 (every-range-helper pred (funcall update curr) end update))))

(defun every-range (pred start end)
  (if (< start end)
      (every-range-helper pred start end '1+)
      (every-range-helper pred start end '1-)))

(defun reduce-range-helper (func curr end acc update)
  (cond ((eql curr end)
	 acc)
	(t
	 (reduce-range-helper func (funcall update curr) end (funcall func acc curr) update))))

(defun reduce-range (func start end &optional (init nil))
  (if (< start end)
      (reduce-range-helper func start end init '1+)
      (reduce-range-helper func start end init '1-)))
