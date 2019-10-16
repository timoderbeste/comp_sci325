(defun make-balance (initial-balance)
  (let ((balance initial-balance))
    (lambda (&optional (update 0))
      (if (= update 0)
	  balance
	  (incf balance update)))))
