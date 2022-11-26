;;; nabeatsu.el --- nabeatsu.el -*- lexical-binding: t; coding: utf-8 -*-
;; Author: 荻野亮
;; Version:
;; Package-Requires: ()
;;; Commentary:
;; This program is free software
;;; Code:

(defun nabeatsu (n)
  (loop for i from 1 to n
        collect (let ((i-str (number-to-string i)))
                  (cond
                   ((= (% i 3) 0)
                    (concat i-str "!"))
                   ((string-match-p "3" i-str)
                    (concat i-str "!"))
                   (t i-str)))))

(loop for x in (nabeatsu 100)
      do (message x))

(dolist (x (nabeatsu 100))
  (message (format "%s" x)))
(mapc (lambda (x) (message (format "%s" x))) (nabeatsu 100))





(provide 'nabeatsu)
;;; nabeatsu.el ends here
