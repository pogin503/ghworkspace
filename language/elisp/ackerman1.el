(require 'anaphora)

(setq ack-hash (make-hash-table :test 'equal))

(defun get-ack-val (m n)
  (gethash `(,m ,n) ack-hash))

(defun put-ack-val-and-get (m n)
  (let ((ret (Ack m n)))
      (puthash `(,m ,n) ret ack-hash)
      ret))

(defun Ack (m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (Ack (- m 1) 1))
        (t
         (Ack (- m 1) (Ack m (- n 1))))))

(defun put-ack-val-and-get2 (m n)
  (let ((ret (Ack2 m n)))
      (puthash `(,m ,n) ret ack-hash)
      ret))

(defun Ack2 (m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (aif (get-ack-val (- m 1) 1)
                     it
                   (put-ack-val-and-get2 (- m 1) 1)))
        (t (let  ((ret (aif (get-ack-val m (- n 1))
                           it
                         (put-ack-val-and-get2 m (- n 1)))))
             (aif (get-ack-val (- m 1) ret)
                 it
               (put-ack-val-and-get2 (- m 1) ret))))))

         ;; (Ack2 (- m 1) (Ack2 m (- n 1))))))

(Ack2 3 5)

(defun put-ack-val-and-get3 (m n)
  (let ((ret (Ack3 m n)))
      (puthash `(,m ,n) ret ack-hash)
      ret))

(defun Ack3 (m n)
  (let ((ack-aux (lambda (m1 n1)
                   (aif (get-ack-val m1 n1)
                       it
                     (put-ack-val-and-get3 m1 n1)))))
    (cond ((= m 0) (+ n 1))
          ((= n 0) (funcall ack-aux (- m 1) 1))
          (t (let  ((ret (loop for i from 0 to (- n 1) do
                               (funcall ack-aux m (- n 1))
                               finally (return (funcall ack-aux m (- n 1))))))
               (funcall ack-aux (- m 1) ret))))))

(setq f (lambda (x) (+ x 1)))
(funcall f 1)

;; (Ack2 (- m 1) (Ack2 m (- n 1))))))

(Ack3 3 6)

(hash-table-size ack-hash)
(get-ack-val 3 8)
(puthash '(3 8) (Ack3 3 8) ack-hash)

(defun calc-Ack (m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (get-ack-val (- m 1) 1))
        (t
         (aif (get-val-ack m n)

               ret))))

;; m=0 -> n+1
;; n=0 -> (Ack (m-1) 1)
;; otherwise (Ack (m-1) (Ack m (n-1)))
;; キャッシュ的な感じに存在するデータを取得して計算を軽減したい
;; 先に入れ子のAck (Ack m,n-1) を計算して蓄えて (Ack m-1,(Ack m,n-1))
;; を計算する
;; get-val-ackで撮ってきたものを値として返したい
;; n=0 if (get-ack-val m-1,1) != nil then it else put (Ack (- m 1) 1)
;;     if (get-val-ack m,n-1) != nil
;;        then it else (Ack m,n-1) and put-ack-val m,n-1

;; Ack m n
;; m=0 n+1
;; n=0 if get-ack-val m-1,1 != nil then it else (put-ack-val (Ack m-1,n))
;; t      nest <- get-ack-val m,n-1 != nil then it else (Ack m,n-1)
;;        get-ack-val

;; ret が大きいとどうしてもスタックオーバーフローしてしまうから
;; ret を0からAckしていってデータを格納して最終的にretの値をやれば
;; スタックオーバーフローはおきないかも

(calc-Ack 4 1)

(Ack 1 1)
(put-ack-val-and-get 1 1)

(defun put-ack-key-val (m n ret)
  (puthash `(,m ,n) ret ack-hash))

(put-ack-val 1 1)

(let ((m 0) (n 0))
  (if (null (get-ack-val m n))
      (put-ack-val m n)
    (get-ack-val m n)))

(put-ack-val 0 0)
(put-ack-val 1 0)
(put-ack-val 1 1)
(put-ack-val 2 0)
(put-ack-val 2 0)
(put-ack-val 2 0)
(put-ack-val 2 0)
(put-ack-val 2 0)
(put-ack-val 2 0)

;; (mapcar #'(lambda (m) (mapcar #'(lambda (n) (put-ack-val m n)))))

(loop for i from 0 to 3 do
      (loop for j from 0 to 3 do
            (put-ack-val i j)))


(maphash #'(lambda (key val)
         (insert (format "key=>%S,value=>%S\n" key val))) ack-hash)

(remhash 'ack ack-hash)
