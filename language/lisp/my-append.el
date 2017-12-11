;;; my-append.el --- my-append.el -*- lexical-binding: t; coding: utf-8 -*-
;; Author: Ryo
;;; Commentary:
;; This program is free software
;;; Code:


(setq lst1 (progn
             (dotimes (num 10)
               (setq lst (append lst (list (car (benchmark-run 100000
                                                  (append '(2 3 4) '(1) )))))))
             lst))

(setq lst2 (progn
             (dotimes (num 10)
               (setq lst (append lst (list (car (benchmark-run 100000
                                                  (append '(1) '(2 3 4))))))))
             lst))

(let* ((slst
        '(0.013151
          0.016582
          0.013496999999999999
          0.017431
          0.015332000000000002
          0.015258
          0.109733
          0.012219
          0.008128
          0.010625000000000002))
       (len-slst (length slst)))
  (/ (apply '+ slst) len-slst))

(provide 'my-append)
;;; my-append.el ends here
