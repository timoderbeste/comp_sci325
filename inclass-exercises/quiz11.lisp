(in-package :cs325-user)

(defvar *facts*
  '(
    (canary isa bird) (tweety isa canary) (tweety isa pet)
    (bird isa animal) (penguin isa bird) (willy isa penguin)
    (animal can-fly no) (pet can-fly no)(bird can-fly yes)
    (penguin can-fly yes) (canary color yellow)
    (penguin color black-and-white) 
    (tweety owner granny)
    ))


(defun foo (x)
  (let ((lst (remove x *facts* :key 'car :test-not 'eql)))
    (list* x
           (mapcar 'caddr (remove 'isa lst :key 'cadr :test-not 'eql))
           (mapcar 'cdr (remove 'isa lst :key 'cadr)))))
