;;; shadchen-sample.el --- shadchen-sample.el
;;; Commentary:
;; This program is free software
;;; Code:

(use-package shadchen
  :load-path "/Users/OginoRyo/.emacs.d/.cask/24.3.1/elpa/shadchen-20141102.1039/"
  :ensure shadchen)

(require 'shadchen)

(defun my-second (lst)
  (match lst
   ((cons _ (cons x rest)) x)))

(match "bobcatdog"
 ((concat
   (and (or "bobcat" "cat") which)
   "dog") which))
"bobcat"

(match 1
  ((1 (message "arg is 1"))
   (2 (message "arg is 2"))
   (_ (message "not match"))))

(match
   (number-sequence 1 10)
 ((append (list 1) _ (list y)) y))

(my-second '(1 2 3))

(match (list 1 2 3)
  ((` (1 (UQ x) 2)) x))

(match-let
  ((x 10)
   (y 11))
 (+ x y))

(match-let
  ((x z)
   (y 11))
 (+ x y))

(macroexpand '(defun-match- vocalize ((list-rest 'cat properties))
	 "Cat vocalization."
	 (message "Meow")))

(defun-match vocalize ((list-rest 'dog properties))
  "Dog vocalization."
  (message "Woof"))

(vocalize '(cat :name tess))

(vocalize '(dog :name bowzer))

(provide 'shadchen-sample)
;;; shadchen-sample.el ends here
