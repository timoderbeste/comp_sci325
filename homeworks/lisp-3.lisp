(defun make-balance-old (initial-balance)
  (let ((balance initial-balance))
    (lambda (&optional (update 0))
      (if (= update 0)
	  balance
	  (incf balance update)))))

(defun make-balance (balance)
  (lambda (&optional (update 0) (b balance))
    (incf balance update)
    balance))
