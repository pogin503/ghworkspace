;;; my-eieio-sample.el --- my-eieio-sample.el
;; Author: Ryo
;; Version:
;; Package-Requires: ()
;;; Commentary:
;; This program is free software
;;; Code:

(require 'eieio)
;; クラスの定義
;; Macro: defclass class-name superclass-list slot-list &rest
;;           options-and-doc
(defclass record ()						; No superclasses

  ((name
	;; :initarg
	:initarg :name
	;; 初期値
	:initform ""
	;; 型
	:type string
	;;
	:custom string
	;; docstring
	:documentation "The name of a person.")
   (birthday
	:initarg :birthday
	:initform "Jan 1, 1970"
	:custom string
	:type string
	:documentation "The person's birthday.")
   (phone :initarg :phone
		  :initform ""
		  :documentation "Phone number."))
  "A single record for tracking people I know.")

 ;; -- Function: CLASS-NAME-p object
 ;;     Return `t' if OBJECT is of the class CLASS-NAME.


 ;; -- Function: CLASS-NAME-child-p object
 ;;     Return `t' if OBJECT is of the class CLASS-NAME, or is of a
 ;;     subclass of CLASS-NAME.

;; メソッドの定義
;;
(defmethod call-record ((rec record) &optional scriptname)
         "Dial the phone for the record REC.
     Execute the program SCRIPTNAME to dial the phone."
       (message "Dialing the phone for %s"  (oref rec name))
	   (shell-command (concat (or scriptname "dialphone.sh")
							  " "
							  (oref rec phone))))
;; call-record
;; (defclass record ()
;;   () "Doc string")
;; record
;; (make-instance 'record)

;; [object record "record"]

;; (record "test" :value 3 :references nil)

;; record
(setq rec (record "Eric" :name "Eric" :birthday "june" :phone "555-5555"))

[object record "Eric" "Eric" "june" "555-5555"]

(call-record rec "my-call-script")

(provide 'my-eieio-sample)
;;; my-eieio-sample.el ends here
