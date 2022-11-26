;;;; omm-mode.el
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) pogin 


;; Author: pogin
;; Maintainer: pogin
;; Keywords: convenience, frames
;; Created: 2011/07/04
;; Version: 0.1
;; URL: 
;; Site: http://d.hatena.ne.jp/pogin/

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


;;; Commentary:
;;
;; Tested on Emacs 23.
;;

;;; Commands:
;;
;; `omm-mode-toggle'
;; toggle omm-mode on/off
;;
;; `omm-mode-style-on'
;; omm-mode-style on

;;; Keybinds
;;
;; C-c C-o: omm-mode-toggle

;;; Customizable Options:
;;
;; None
;;



;;; Change log:
;;
;; Created:  2011/07/04

;;; TODO
;;
;; play-music function
;; Change Background Picture function?
;; All buffers mode-line to bunish
;; make Timer function 
 

;;; Code
(defvar omm-mode-line-conf-list mode-line-format)
;;omm-mode-line-conf-list

(defvar omm-mode-toggle-var nil
  "If omm-mode-toggle-var variable is nil, omm-mode is off.
If you eval omm-mode-toggle function, omm-mode is on.
If this variable is t, omm-mode is on.
If you eval omm-mode-toggle, omm-mode-toggle-var change nil")

(defconst omm-init-list-flag 
  (list 
     (eq linum-mode t)
     (eq scroll-bar-mode t)
     (eq tool-bar-mode t)
     (eq menu-bar-mode t)
     omm-mode-line-conf-list
    (if (fboundp 'elscreen-mode)
        (list t (eq elscreen-mode t))
      (list nil nil))
    (if (fboundp 'tabbar-mode)
        (list t (eq tabbar-mode t))
      (list nil nil))
    ))


;; (setq omm-init-list-flag nil)
;; (defun omm-init-list ()
;;   (progn
;;  (setq omm-init-list-flag (push (eq linum-mode t)       omm-init-list-flag))
;;  (setq omm-init-list-flag (push (eq scroll-bar-mode t)  omm-init-list-flag))
;;  (setq omm-init-list-flag (push (eq tool-bar-mode t)    omm-init-list-flag))
;;  (setq omm-init-list-flag (push (eq menu-bar-mode t)    omm-init-list-flag))
;;  (setq omm-init-list-flag (push omm-mode-line-conf-list omm-init-list-flag))
;;  (if (fboundp 'elscreen-mode)
;;      (setq omm-init-list-flag (push (list t (eq elscreen-mode t))
;;                                     omm-init-list-flag))
;;    (setq omm-init-list-flag (push (list nil nil)
;;                                   omm-init-list-flag)))
;;  (if (fboundp 'tabbar-mode)
;;      (setq omm-init-list-flag (push (list t (eq tabbar-mode t))
;;                                     omm-init-list-flag))
;;    (setq omm-init-list-flag (push (list nil nil)
;;                                   omm-init-list-flag)))
;;  (setq omm-init-list-flag (reverse omm-init-list-flag))
;;  ))


;; (defvar omm-init-flag nil)
;; (if (eq omm-init-flag nil)
;;  (progn
;;    (omm-init-list)
;;    (setq omm-init-flag t)))
;; (setq omm-init-flag nil)

;; (omm-init-list)
;; omm-init-list-flag

;; (pop omm-init-list-flag)
(defun omm-mode-style-on ()
  (interactive)
  (progn
    (linum-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (setq mode-line-format nil)
    (if (fboundp 'elscreen-mode)
        (elscreen-mode -1))
    (if (fboundp 'tabbar-mode)
        (tabbar-mode -1))
    (setq omm-mode-toggle-var t)))

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
    (setq omm-mode-toggle-var nil)))

(defun omm-mode-toggle ()
  (interactive)
  (if (eq omm-mode-toggle-var t)
      (omm-mode-style-off)
    (omm-mode-style-on)))

(defvar omm-mode-map nil)
(unless omm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-o" 'omm-mode-toggle)
    (setq omm-mode-map map)))

(define-minor-mode omm-mode
  :keymap omm-mode-map
  :group omm-mode)

(provide 'omm-mode)

;;; filename ends here

