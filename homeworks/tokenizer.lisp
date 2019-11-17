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
	((and (= curr-index (length str)) (not (eql delim #\space))) curr-index)
	((and (< curr-index (length str)) (eql #\space delim))
	 (position delim str :test-not #'equal :start curr-index))
	((< curr-index (length str)) curr-index)
	(t nil)))

(defun next-token-end (delim str start)
  (position delim str :start start))


(defun make-tokenizer (str &optional (delim #\space))
  (make-instance 'tokenizer :str str :delim delim :curr-index (if (eql delim #\space) (next-token-start delim str 0) 0)))

(defmethod next-token-p ((tr tokenizer))
  (curr-index tr))

(defmethod next-token (tr)
  (cond ((and (not (next-token-p tr)) (eql (delim tr) #\space)) nil)
	((and (not (next-token-p tr)) (not (eql (delim tr) #\space))) "")
	(t
	 (let* ((start (next-token-start (delim tr) (str tr) (curr-index tr)))
		(end (next-token-end (delim tr) (str tr) start)))
	   (setf (curr-index tr) (next-token-start (delim tr) (str tr) (and end (1+ end))))
	   (subseq (str tr) start end)))))
