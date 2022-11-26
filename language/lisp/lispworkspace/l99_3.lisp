(defun element-at1 (lst num &optional (i 1))
  (if (null lst)
      nil
     (if (= num i)
	 (car lst)
       (element-at1 (cdr lst) num (+ 1 i))))

(defun element-at2 (lst num)
  (nth (1- num) lst))
;110105(l-99)
;nthでやるやり方
;dotimesで繰り返すやり方
;loopでやるやり方
;自分でやるやり方