(in-package :cs325-user)


(define-test query-graph
  (assert-equal
    '(
      ((?name "karen") (?y karen) (?x john))
      ((?name "alex") (?y alex) (?x karen))
      )
    (query-graph '((?x knows ?y) (?y name ?name))))
  )

;;; Implementation

;;; create a namespace package for rdfs:class, rdfs:property, etc, which
;;; appear as items in some triples
(defpackage :rdfs
  (:export :class :comment :label :property :sub-class-of :sub-property-of)
  )

(defvar *facts*
  '((john knows karen) (karen knows alex)
    (karen name "karen") (alex name "alex")
    (phil knows phil)
    ))

(defun load-graph (file)
  (setq *facts* nil)
  (with-open-file (in file)
    (do ((fact (read in nil) (read in nil)))
        ((null fact) (length *facts*))
      (push fact *facts*))))

(defun query-graph (triples &optional (blists '(())))
  (cond ((null triples) blists)
        ((null blists) nil)
        (t (query-graph (cdr triples)
                        (query-triple (car triples) blists)))))

(defun query-triple (query &optional (blists '(())))
  (mapcan (lambda (fact) (match query fact blists))
          *facts*))

(defun match (query fact &optional (blists '(())))
  (cond ((null query) blists)
        (t
         (match (cdr query) (cdr fact)
                (match-part (car query) (car fact) blists)))))

(defun match-part (x y blists)
  (cond ((eql x y) blists)
        ((not (var-p x)) nil)
        (t (update-blists x y blists))))

(defun update-blists (var item blists)
  (mapcan (lambda (blist)
            (let ((new-blist (update-blist var item blist)))
              (and new-blist (list new-blist))))
          blists))

(defun update-blist (var item blist)
  (let ((binding (assoc var blist)))
    (cond ((null binding) (cons (list var item) blist))
          ((eql (cadr binding) item) blist)
          (t nil))))

(defun var-p (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))