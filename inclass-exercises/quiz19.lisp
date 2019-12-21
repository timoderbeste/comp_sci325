(in-package :sddr-tests)

(defparameter *maze-kb*
  '(
    ;; The empty plan works if current state = goal state
    (<- (plan nil ?goal ?goal))
    
    ;; The plan (cons action actions) works if 
    ;;   - the result of action in state1 is state2
    ;;   - the remaining actions work for state2
    (<- (plan (cons ?action ?actions) ?state1 ?goal)
        (results ?action ?state1 ?state2)
        (plan ?actions ?state2 ?goal))
    
    ;; The results of MOVE in different rooms
            
    (<- (results (move e) (location a) (location b)))
    (<- (results (move w) (location a) (location c)))
    (<- (results (move n) (location b) (location d)))
    (<- (results (move s) (location b) (location e)))
    (<- (results (move w) (location c) (location b)))
    ))

(defun plan (start end)
  (let ((query `(plan ?plan (location ,start) (location ,end))))
    (mapcar 'cadr (ask query *maze-kb*))))
