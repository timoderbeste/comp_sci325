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
    :accessor trie-word)))

(defmethod make-trie ()
  (make-instance 'trie))

(defmethod add-word-helper (word idx (root trie))
  (cond ((= idx (length word))
	 (setf (trie-word root) word)
	 root)
	(t
	 (let ((curr-char (char word idx)))
	   (unless (assoc curr-char (trie-children root))
	     (push (cons curr-char (make-trie)) (trie-children root)))
	   (add-word-helper word (1+ idx) (cdr (assoc curr-char (trie-children root))))))))

(defmethod add-word (word (root trie))
  (add-word-helper word 0 root))

(defmethod subtrie ((root trie) &rest chars)
  (if (null chars)
      root
      (let ((child (cdr (assoc (car chars) (trie-children root)))))
	(if child
	    (apply #'subtrie child (cdr chars))
	    nil))))

(defmethod trie-count ((root trie))
  (let ((curr-count (if (trie-word root) 1 0)))
    (if (null root)
	curr-count
	(do ((rest-children (trie-children root) (cdr rest-children))
	     (count curr-count (+ count (trie-count (cdr (car rest-children))))))
	    ((null rest-children) count)))))


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
