;;; marker --- marker
;; This program is free software
;;; Commentary:
;;; Code:

(defun region-test (s e)
  (interactive "r")
  (message "%dから%dまでがリージョンです (point=%d mark=%d)"
	   s e
	   (if (= (point) s) s e)
	   (if (= (point) s) e s)))

(defun paren-region (s e)
  "リージョンを括弧で囲む。
`S' は始点?rea
`E' は終点?"
  (interactive "r")
  (setq e (set-marker (make-marker) e))
  (goto-char s)
  (insert "(")
  (goto-char e)
  (insert ")"))

(provide 'marker)
;;; marker ends here
