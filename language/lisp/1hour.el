;;; 1hour.el --- 1hour.el -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;; This program is free software
;;; Code:

(let ((lst '(1 2 3 4 5))
      (n 0))
  (cl-loop for i from 0 to (- (length lst) 1) do
        (setq n (+ n (nth i lst))))
  (print n))

(defun my-rec-sum (lst n)
  (if (not (null lst))
      (my-rec-sum (cdr lst) (+ n (car lst)))
    n))

(my-rec-sum '(1 2 3 4 5) 0)

(defun my-flatten (lst)
  (cond ((null lst) nil)
        ((atom lst) (list lst))
        (t (append (my-flatten (car lst)) (my-flatten (cdr lst))))))

(my-flatten '(1 2 (1 2 3) (1 2 . 3)  (1 2 (1 . 2) 2 (3 . 2) nil)))
(1 2 1 2 3 1 2 3 1 2 1 2 2 3 2)



(defun my-zip-and-flatten (lst1 lst2 ret)
  (if (or (null lst1) (null lst2))
      ret
    (my-zip-and-flatten (cdr lst1) (cdr lst2)
                        (if (null ret)
                            (list (car lst1) (car lst2))
                          (append ret (list (car lst1) (car lst2)))))))

;;; 1hour.el ends here
