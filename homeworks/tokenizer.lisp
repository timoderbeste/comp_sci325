(defclass tokenizer ()
  ((str
    :initarg :str
    :accessor str)
   (delim
    :initarg :delim
    :initform #\space
    :accessor delim)))

(defun make-tokenizer (str &optional (delim #\space))
  (make-instance 'tokenizer :str str :delim delim))


(defun next-index (tr-delim tr-str)
  (let ((next-token-index (find-if-not (lambda (c) (equal c tr-delim)) tr-str)))
    (if (null next-token-index)
	nil
	(search (format nil "~C" (find-if-not (lambda (c) (equal c tr-delim)) tr-str)) tr-str))))

(defmethod next-token-p ((tr tokenizer))
  (if (eql (delim tr) #\space)
      (not (null (next-index (delim tr) (str tr))))
      (not (null (str tr)))))

(defun extract-token-space (tr-delim tr-str)
  (if (or (= 0 (length tr-str)) (null tr-str))
      (values nil nil)
      (let ((next-token-index (next-index tr-delim tr-str)))
	(if (null next-token-index)
	    (values nil nil)
	    (let*((cutted-str (subseq tr-str (next-index tr-delim tr-str)))
		  (next-delim-index (search (format nil "~C" tr-delim) cutted-str))
		  (token (subseq cutted-str 0 next-delim-index))
		  (rest-str (if (null next-delim-index) "" (subseq cutted-str next-delim-index))))
	      (values token rest-str))))))

(defun extract-token-other (tr-delim tr-str)
  (if (= 0 (length tr-str))
      (values "" nil)
      (let ((next-delim-index (search (format nil "~C" tr-delim) tr-str)))
	(if (null next-delim-index)
	    (values tr-str nil)
	    (values (subseq tr-str 0 next-delim-index) (subseq tr-str (1+ next-delim-index)))))))

(defmethod next-token ((tr tokenizer))
  (if (eql #\space (delim tr))
      (multiple-value-bind (token rest-str) (extract-token-space (delim tr) (str tr))
	(setf (str tr) rest-str)
	token)
      (multiple-value-bind (token rest-str) (extract-token-other (delim tr) (str tr))
	(setf (str tr) rest-str)
	token)))

(defun split-string (str &optional (delim #\space))
  (let ((tr (make-tokenizer str delim)))
    (do ((l nil (cons (next-token tr) l)))
	((not (next-token-p tr)) (nreverse l)))))
