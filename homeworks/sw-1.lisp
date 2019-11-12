(defun camelize (str &optional (capitalize nil))
  (let ((fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
      (do* ((start 0 (if end (1+ end) nil))
	    (end (position #\- str) (if start (position #\- str :start start) nil)))
	   ((null start)
	    fstr)
	(if (or (/= start 0) capitalize)
	    (format s (string-capitalize (subseq str start end)))
	    (format s (subseq str start end)))))))


(defun hyphenate-loop-helper (str i s)
  (let ((p (if (= 0 i) nil (aref str (1- i))))
	(c (aref str i)))
    (cond ((or (and (null p) (upper-case-p c))
	       (and (not (null p)) (upper-case-p c) (upper-case-p p)))
	   (format s "~A" c))
	  ((upper-case-p c)
	   (format s "-~A" c))
	  (t (format s "~A" c)))))

(defun hyphenate (str &optional (case :upper))
  (let ((fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
      (do ((i 0 (1+ i)))
	  ((= (length str) i)
	   (if (eql case :upper)
	       (string-upcase fstr)
	       (string-downcase fstr)))
	(hyphenate-loop-helper str i s)))))
