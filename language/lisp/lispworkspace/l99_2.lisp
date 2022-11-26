(defun my-but-last (lst)
  (cond
    ((single lst) lst)
    ((= (length lst) 2)
     (cons (car lst) (my-but-last (cdr lst))))
    (t (my-but-last (cdr lst)))
    )
  )

(defun single (lst)
  (and (consp lst) (not (cdr lst))))
