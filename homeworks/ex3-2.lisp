(defun stable-union (set1 set2)
  (cond ((null set2)
	 set1)
	((member (car set2) set1)
	 (stable-union set1 (cdr set2)))
	(t
	 (stable-union (nconc set1 (list (car set2))) (cdr set2)))))
