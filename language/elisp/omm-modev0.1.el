;;;; omm-mode.el
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) pogin 


;; Author: pogin
;; Maintainer: pogin
;; Keywords: 
;; Created: 2011/07/04
;; Version: 0.1
;; URL: 
;; Site: 


(defvar omm-mode-version nil)
(setq omm-mode-version "0.1")

;;; License
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Installation:
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'omm-mode)
;;
;; If you call drill instructor for Emacs, M-x drill-instructor.
;;
;; If you call him always put the following expression
;; (setq drill-instructor-global t)



;;; Commentary:
;;
;; Tested on Emacs 23.

;;; Commands:
;;
;; `omm-mode-toggle'
;; toggle omm-mode on/off
;;
;; `omm-mode-style-on'
;; omm-mode-style on

;;; Customizable Options:
;;
;; None
;;



;;; Change log:
;;
;; Create

;;; TODO
;;
;; make: plya-music function
;; make: Change Background Picture function?
 

;;; Code
(defvar omm-mode-line-conf-list mode-line-format)
;;omm-mode-line-conf-list

(defvar omm-toggle-var nil
  "If this variable is nil, omm-mode is off.
If you eval omm-mode-style function, omm-mode on.
If this variable is t, omm-mode is on.
If you eval omm-mode-toggle, omm-toggle-var change nil")

;; (defvar omm-init-flag
;;    '((eq linum-mode t)
;;    (eq scroll-bar-mode t)
;;    (eq tool-bar-mode t)
;;    (eq menu-bar-mode t)
;;    omm-mode-line-conf-list
;;    (if (fboundp 'elscreen-mode)
;; 	   (list t (eq elscreen-mode t))
;; 	 (list nil nil))
;;    (if (fboundp 'tabbar-mode)
;; 	   (list t (eq tabbar-mode t))
;; 	 (list nil nil))))

;; omm-init-flag

;; (defconst kkk (list t t t))
;; kkk

(defvar omm-init-list-flag 'omm-init-list)
;; (setq omm-init-list-flag nil)

;; omm-init-list-flag

(defun omm-init-list ()
  (progn
	(setq omm-init-list-flag (push (eq linum-mode t) omm-init-list-flag))
	(setq omm-init-list-flag (push (eq scroll-bar-mode t) omm-init-list-flag))
	(setq omm-init-list-flag (push (eq tool-bar-mode t) omm-init-list-flag))
	(setq omm-init-list-flag (push (eq menu-bar-mode t) omm-init-list-flag))
	(setq omm-init-list-flag (push omm-mode-line-conf-list omm-init-list-flag))
	(if (fboundp 'elscreen-mode)
		(setq omm-init-list-flag (push (list t (eq elscreen-mode t))
									   omm-init-list-flag))
	  (setq omm-init-list-flag (push (list nil nil)
									 omm-init-list-flag)))
	(if (fboundp 'tabbar-mode)
		(setq omm-init-list-flag (push (list t (eq tabbar-mode t))
									   omm-init-list-flag))
	  (setq omm-init-list-flag (push (list nil nil)
									 omm-init-list-flag)))
	(setq omm-init-list-flag (reverse omm-init-list-flag))
	;; (setq omm-init-list-flag omm-init-list-flag)
	))

;; (omm-init-list)
;; omm-init-list-flag


;;omm-init-list
;; (linum-mode)
;; (defvar omm-mode-)

(defun omm-mode-style-on ()
  (interactive)
  (progn
	(linum-mode -1)
	(scroll-bar-mode -1)
	(tool-bar-mode -1)
	(menu-bar-mode -1)
	(setq mode-line-format nil)
	(if (fboundp 'elscreen-mode)
		(elscreen-mode nil))
	(if (fboundp 'tabbar-mode)
		(tabbar-mode nil))
	(setq omm-toggle-var t)))
;; omm-toggle-var

;; (defun omm-mode-init ()
;;   (progn
;; 	(omm-mode-cons line-number-mode)
;; 	(omm-mode-cons scroll-bar-mode)
;; 	(omm-mode-cons tool-bar-mode)
;; 	(omm-mode-cons menu-bar-mode)
;; 	(omm-mode-cons )))

(defun omm-mode-style-off ()
  (progn
	(if (eq (nth 0 omm-init-list-flag) t)
		(linum-mode 1))
	(if (eq (nth 1 omm-init-list-flag) t)
		(scroll-bar-mode 1))
	(if (eq (nth 2 omm-init-list-flag) t)
		(tool-bar-mode 1))
	(if (eq (nth 3 omm-init-list-flag) t)
		(menu-bar-mode 1))
	(setq mode-line-format omm-mode-line-conf-list)
	(if (and (eq (first (nth 5 omm-init-list-flag)) t)
			 (eq (second (nth 5 omm-init-list-flag)) t))
		(elscreen-mode t))
	(if (and (eq (first (nth 6 omm-init-list-flag)) t)
			 (eq (second (nth 6 omm-init-list-flag)) t))
		(tabbar-mode t))
	(setq omm-toggle-var nil)))

(defun omm-mode-toggle ()
  (interactive)
  (if (eq omm-toggle-var t)
	  (omm-mode-style-off)
	(omm-mode-style-on)))



;; (progn
;;   (linum-mode t)
;;   (scroll-bar-mode t)
;;   (tool-bar-mode t)
;;   (menu-bar-mode t)
;;   (tabbar-mode t))

;; (omm-mode-style-on)
;; (omm-mode-toggle)


