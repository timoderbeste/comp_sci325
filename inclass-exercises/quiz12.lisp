(in-package :cs325-user)

(defparameter *frames*
  '(
    (animal nil (can-fly no))
    (bird (animal) (can-fly yes))
    (canary (bird) (color yellow))
    (penguin (bird) (can-fly no) (color black-and-white))
    (pet nil (can-fly no))
    (tweety (canary pet) (owner granny))
    (willy (penguin)))
  )

(defun foo (x)
  (cons x (mapcan 'foo (copy-list (cadr (assoc x *frames*))))))

