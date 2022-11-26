;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(9 3 5 8 2 5 6)

(defun bubble-sort (lst)
  (for i 0 (length lst)
       (b-sort lst)))
;;bubble-sort

;;(bubble-sort '(9 3 5 8 2 5 6))





(defun b-sort (lst)
  (let ((t1 (car lst)) (t2 (car (cdr lst))) (swap 0))
    (if (> t1 t2)
	((setf swap t1)
	 (setf t1 t2)
	 (setf t2 swap)))
    (cons t1
	  (progn ((null (rest lst) nil))
		 (t (cons t2 (b-sort (rest lst)))))))))


;;bubble-sort


(defun bubble-sort (lst)
  (while (progn ((null (cdr lst) nil)
	  ()b-sort))

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

