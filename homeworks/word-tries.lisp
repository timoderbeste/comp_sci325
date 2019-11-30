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
  (if (= idx (length word))
      (progn
	(setf (trie-word root) word)
	root)
      (let ((curr-char (char word idx)))
	(when (not (assoc curr-char (trie-children root)))
	  (push (cons curr-char (make-trie)) (trie-children root)))
	(add-word-helper word (1+ idx) (cdr (assoc curr-char (trie-children root)))))))

(defmethod add-word (word (root trie))
  (add-word-helper word 0 root))

(defmethod subtrie ((root trie) &rest chars)
  (if (null chars)
      root
      (let ((child (cdr (assoc (car chars) (trie-children root)))))
	(if child
	    (apply #'subtrie child (cdr chars))
	    nil))))
