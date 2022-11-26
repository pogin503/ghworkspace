;;; haskell-matrix.el --- haskell-matrix.el -*- lexical-binding: t; coding: utf-8 -*-
;; Author: iMac
;;; Commentary:
;; This program is free software
;;; Code:


(require 'haskell-emacs)
(haskell-emacs-init)

(Matrix.identity 3)
;; ((1 0 0) (0 1 0) (0 0 1))

(Matrix.transpose '((1 2) (3 4) (5 6)))
;; ((1 3 5) (2 4 6))

(Matrix.isIdentity '((1 0) (0 1)))
;; t

(Matrix.dyadic '(1 2 3) '(4 5 6))
((4 5 6) (8 10 12) (12 15 18))

(provide 'haskell-matrix)

;;; haskell-matrix.el ends here
