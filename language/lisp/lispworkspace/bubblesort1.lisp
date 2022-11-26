(defun bubble-sort (lst)
  (for (i 0 (length lst))
       (b-sort lst)))

(defun b-sort (lst)
  (let ((t1 (car lst)) (t2 (car (cdr lst))) (swap 0))
    (if (> t1 t2)
        ((setf swap t1)  ;;swapする
         (setf t1 t2)
         (setf t2 swap)))
    (cons t1
          (cond ((null (rest lst) nil))
                 (t (cons t2 (b-sort (rest lst))))))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
       ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
	 ,@body)))

