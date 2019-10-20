(in-package :cs325-user)


(define-test update-blist
  (assert-equal '((?x a)) (update-blist '?x 'a nil))
  (assert-equal '((?x a)) (update-blist '?x 'a '((?x a))))
  (assert-equal '((?z c) (?y b) (?x a)) (update-blist '?z 'c '((?y b) (?x a))))
  (assert-equal '() (update-blist '?x 'c '((?y b) (?x a))))
  )

(defun update-blist (var item blist)
  (let ((binding (assoc var blist)))
    (cond ((null  binding) (cons (list var item) blists))
	  ((eql (cadr binding) item) blist)
	  (t nil))))

(defun my-update-blist (var item blists)
  (if (assoc var blists)
      (if (not (eql (cdr (assoc var blists)) (list item)))
	  nil
	  (list var item))
      (cons (list var item) blists)))
