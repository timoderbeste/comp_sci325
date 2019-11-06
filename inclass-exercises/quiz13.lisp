(in-package :cs325-user)

(defparameter *frames*
  '(
    (day-boat (boat) (nav-zone 5)) 
    (wheel-boat (boat) (nav-zone 100))
    (engineless-boat (day-boat))
    (small-multi-hull-boat (day-boat))
    (pedal-wheel-boat (engineless-boat wheel-boat))
    (small-catamaran (small-multi-hull-boat))
    (pedalo (pedal-wheel-boat small-catamaran))
    ))

;;; from quiz12
(defun get-frame (name) (assoc name *frames*))
(defun get-absts (name) (cadr (assoc name *frames*)))
(defun get-filler (name prop) (assoc prop (cddr (get-frame name))))

(defun linearize (x)
  (cons x 
        (remove-duplicates
         (mapcan 'linearize (copy-list (get-absts x))))))

(defparameter *absts*
  (mapcar 'linearize (mapcar 'car *frames*)))

(defun get-all-absts (x) (assoc x *absts*))

;;; for quiz13
(defun inherit-filler (x prop)
  (some (lambda (y) (get-filler y prop))
        (get-all-absts x)))

