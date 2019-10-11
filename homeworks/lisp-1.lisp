(defun has-number-p (lst)
  (if (atom lst)
      (numberp lst)
      (some #'has-number-p lst)))
