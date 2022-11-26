;;; hit-and-blow.el --- hit-and-blow.el -*- lexical-binding: t; coding: utf-8 -*-
;; Package-Requires: ()
;;; Commentary:
;; This program is free software
;;; Code:

(require 's)
(require 'dash)

(defun my-s-to-list (str)
  ""
  (-map (lambda (x) (char-to-string x)) (string-to-list str)))

(defun gen-random-num (len)
  ""
  (let (num)
    (setq num (let ((x 0))
                (while (equal x 0)
                  (setq x (random 9)))
                (list x)))
    (while (> len (length num))
      (setq num (-union num (list (random 9)))))
    (s-join "" (-map 'number-to-string num))))

(defun result-hit-and-blow (yans ans)
  ""
  (let ((hit 0) (blow 0) (lst (my-s-to-list yans)))
    ;; (s-contains? 52 )
    (-map (lambda (needle)
            (when (s-contains? needle ans)
              (if (eq (string-match needle ans)
                      (-elem-index needle lst))
                  (setq hit (1+ hit))
                (setq blow (1+ blow)))))
          lst)
    (list hit blow)))

(defun num-hit ()
  ""
  (interactive)
  (read-string (message "start hit-and-blow"))
  (let* (num len ret ans (q "") (count 0))
    (while (not (s-match ":quit" q))
      (setq len (string-to-number (read-from-minibuffer "num length> ")))
      (if (< 10 len)
          (read-string "no! (length < 10)")
        (setq num (gen-random-num len))
        (let ((hit 0) (ok len) yans)
          (while (not (= ok hit))
            (setq yans (read-from-minibuffer "your answer is >"))
            (setq ret (result-hit-and-blow yans num))
            (setq hit (nth 0 ret))
            (read-string (format "hit:%d blow:%d" hit (nth 1 ret)))
            (setq count (1+ count)))
          (read-string (format "congratulations! (count = %d)" count)))
        (setq q (read-from-minibuffer "next num or :quit>"))
        ))
    (message "bye")))

(provide 'hit-and-blow)
;;; hit-and-blow.el ends here
