;;; ackerman --- ackerman -*- lexical-binding: t -*-
;; This program is free software
;;; Commentary:
;;; Code:
(defun Ack (m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (Ack (- m 1) 1))
        (t (Ack (- m 1) (Ack m (- n 1))))))
(provide 'ackerman)
;;; ackerman ends here
