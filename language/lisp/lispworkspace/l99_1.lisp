(defun my-last (lst)
  (if (single lst)
      lst
    (my-last (cdr lst))))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

