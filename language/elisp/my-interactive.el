;;; my-interactive.el --- interactive.el
;; This program is free software
;;; Commentary:
;;; Code:

(defun my-n-interactive (n)
  (interactive "nCount: ") ; ミニバッファで読む
  (insert (number-to-string (* 2 n))))

(defun my-r-interactive (&optional beg end)
  (interactive "r")
  (insert (number-to-string beg) " " (number-to-string end)))


(read-only-mode)
(defun my-asterisk-interactive ()
  (interactive "*"))

(defun my-x-interactive (form)
  (interactive "xInput sexp: ")
  (insert (format "%s" form)))

(defun my-X-interactive (form)
  (interactive "XInput sexp: ")
  (insert (format "%s" form)))

(defun my-U-interactive (u)
  (interactive "u")
  (insert (format "%s" u)))

(defun my-U-interactive (u)
  (interactive "U")
  (insert (format "%s" u)))

(defun my-function-interactive (f)
  (interactive "aInput: ")
  (insert "%s" f))

(defun my-buffer-interactive (b)
  (interactive "b")
  (insert b))

(defun my-character-interactive (c)
  (interactive "cInput character: ")
  (insert c))

(defun my-command-interactive (cmd)
  (interactive "CInput command:")
  (insert (symbol-name cmd)))

(defun my-point-interactive (p)
  (interactive "d")
  (insert (format "%d" p)))

(defun my-directory-interactive (d)
  (interactive "DInput directory: ")
  (insert d))

(defun my-file-iteractive (f)
  (interactive "fInput file: ")
  (insert f))

(defun my-file-not-exist-interactive (f)
  (interactive "FInput file?: ")
  (insert f))

(defun my-file-or-dir-interactive (fd)
  (interactive "GInput: ")
  (insert fd))

(defun my-text-interactive (txt)
  (interactive "sInput text: ")
  (insert txt))

(provide 'my-interactive)
;;; my-interactive.el ends here
