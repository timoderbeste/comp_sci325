(defpackage trie
  (:use :common-lisp))

(defclass trie ()
  ((children
    :initarg :children
    :initform '()
    :accessor trie-children)
   (word
    :initarg :word
    :initform nil
    :accessor trie-word)
   (count
    :initarg :count
    :initform 0
    :accessor trie-count)))

(defmethod make-trie ()
  (make-instance 'trie))

(defmethod get-trie-child (curr-char (root trie) &optional (add nil))
  (cond ((null curr-char)
	 nil)
	(add
	 (unless (assoc curr-char (trie-children root))
	   (push (cons curr-char (make-trie)) (trie-children root))))
	(t
	 (cdr (assoc curr-char (trie-children root))))))

(defmethod add-word-helper (word idx (root trie))
  (incf (trie-count root))
  (cond ((= idx (length word))
	 (setf (trie-word root) word)
	 root)
	(t
	 (let ((curr-char (char word idx)))
	   (get-trie-child curr-char root t)
	   (add-word-helper word (1+ idx) (cdr (assoc curr-char (trie-children root))))))))

(defmethod add-word (word (root trie))
  (add-word-helper word 0 root))

(defmethod subtrie-helper ((root trie) chars)
  (if (null chars)
      root
      (let ((child (get-trie-child (car chars) root)))
	(if child
	    (subtrie-helper child (cdr chars))
	    nil))))

(defmethod subtrie ((root trie) &rest chars)
  (subtrie-helper root chars))
  
(defmethod mapc-trie (fn (root trie))
  (do ((rest-children (trie-children root) (cdr rest-children)))
      ((null rest-children) root)
    (funcall fn (caar rest-children) (cdar rest-children))))

(defmethod read-words (file (root trie))
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
	       (read-line str nil 'eof)))
	((eql line 'eof) root)
      (add-word line root))))

(defmethod print-object ((root trie) out)
  (print-unreadable-object (root out :type t)
    (format out "~A ~A" (trie-word root) (trie-count root))))
