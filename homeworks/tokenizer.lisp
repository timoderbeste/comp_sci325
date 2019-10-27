(defclass tokenizer ()
  ((str
    :initarg :str
    :accessor str)
   (delim
    :initarg :delim
    :initform #\space
    :accessor delim)
   (tokens
    :initarg :tokens
    :initform '()
    :accessor tokens)))

(defun make-tokenizer (str &optional (delim #\space))
  ;;; TODO tokenize string with delim!
  (make-instance 'tokenizer :str str :delim delim))

(defmethod next-token-p ((tr tokenizer)))

(defmethod next-token ((tr tokenizer)))
