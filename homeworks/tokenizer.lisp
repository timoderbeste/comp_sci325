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
  (and curr-index
       (if (eql #\space delim)
	   (position delim str :test-not #'equal :start curr-index)
	   curr-index)))
  
(defun next-token-end (delim str start)
  (position delim str :start start))


(defun make-tokenizer (str &optional (delim #\space))
  (make-instance 'tokenizer :str str :delim delim :curr-index (if (eql delim #\space) (next-token-start delim str 0) 0)))

(defmethod next-token-p ((tr tokenizer))
  (curr-index tr))

(defmethod next-token (tr)
  (if (or (null (curr-index tr)) (= (length (str tr)) (curr-index tr)))
      (values (if (eql (delim tr) #\space) nil "") nil)
      (let* ((start (next-token-start (delim tr) (str tr) (curr-index tr)))
	     (end (next-token-end (delim tr) (str tr) start)))
	(setf (curr-index tr) (next-token-start (delim tr) (str tr) (and end (1+ end))))
	(subseq (str tr) start end))))
