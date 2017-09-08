
;; defstruct
(defstruct foo
  (a 10)
  (b nil)
  c)
;; => foo
(setq z1 (make-foo))
;; => [cl-struct-foo 10 nil nil]

(setq z2 (make-foo :b 20 :c 30))
;;=> [cl-struct-foo 10 20 30]

(foo-a z1)
;;=> 10
(setf (foo-a z1) 100)
;;=> 100

z1
;;=> [cl-struct-foo 100 nil nil]

(setq z3 (copy-foo z2))
;;=> [cl-struct-foo 10 20 30]
(equal z2 z3)
;;=> t
(equalp z2 z3)
;;=> t

(foo-p z1)
;;=> t

(foo-a z1)
;;=> 10
