(defclass tokenizer ()
  ((str
    :initarg :str
    :accessor str)
   (delim
    :initarg :delim
    :initform #\space
    :accessor delim)
   (curr-index
    :initarg :curr-index
    :initform 0
    :accessor curr-index)))

(defun make-tokenizer (str &optional (delim #\space))
  (make-instance 'tokenizer :str str :delim delim))

(defun next-index (tr-delim tr-str tr-curr-index)
  (let ((next-token-init-char (find-if-not (lambda (c) (equal c tr-delim)) tr-str :start tr-curr-index)))
    (if (null next-token-init-char)
	nil
	(position next-token-init-char tr-str :start tr-curr-index))))

(defmethod next-token-p ((tr tokenizer))
  (if (eql (delim tr) #\space)
      (not (null (next-index (delim tr) (str tr) (curr-index tr))))
      (not (null (str tr)))))

(defun extract-token-space (tr-delim tr-str tr-curr-index)
  (if (or (= 0 (length tr-str)) (null tr-str))
      (values nil nil)
      (let ((next-token-index (next-index tr-delim tr-str tr-curr-index)))
	(if (null next-token-index)
	    (values nil nil)
	    (let* ((next-delim-index (position tr-delim tr-str :start next-token-index))
		   (token (subseq tr-str next-token-index next-delim-index)))
	      (values token (1+ tr-curr-index)))))))

(defun extract-token-other (tr-delim tr-str tr-curr-index)
  (if (= 0 (length tr-str))
      (values "" nil)
      (let ((next-delim-index (position tr-delim tr-str)))
	(if (null next-delim-index)
	    (values tr-str nil)
	    (values (subseq tr-str 0 next-delim-index) (subseq tr-str (1+ next-delim-index)))))))

(defmethod next-token ((tr tokenizer))
  (if (eql #\space (delim tr))
      (multiple-value-bind (token next-index-value) (extract-token-space (delim tr) (str tr) (curr-index tr))
	(setf (curr-index tr) next-index-value)
	token)
      (multiple-value-bind (token rest-str) (extract-token-other (delim tr) (str tr) (curr-index tr))
	(setf (str tr) rest-str)
	token)))

(defun split-string (str &optional (delim #\space))
  (let ((tr (make-tokenizer str delim)))
    (do ((l nil (cons (next-token tr) l)))
	((not (next-token-p tr)) (nreverse l)))))
