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
    :initform nil
    :accessor curr-index)))

(defun next-token-start (delim str curr-index)
  (cond ((not curr-index) nil)
	((eql #\space delim) (position delim str :test-not #'equal :start curr-index))
	(t curr-index)))

(defun next-token-end (delim str start)
  (position delim str :start start))

(defun make-tokenizer (str &optional (delim #\space))
  (make-instance 'tokenizer :str str :delim delim :curr-index  -1))

(defmethod next-token-p ((tr tokenizer))
  (next-token-start (delim tr) (str tr) (curr-index tr)))

(defmethod next-token (tr)
  (if (next-token-p tr)
      (let* ((start (next-token-start (delim tr) (str tr) (curr-index tr)))
	     (end (next-token-end (delim tr) (str tr) start)))
	(setf (curr-index tr) (next-token-start (delim tr) (str tr) (and end (1+ end))))
	(subseq (str tr) start end))
      nil))
