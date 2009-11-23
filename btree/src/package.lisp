(defpackage :btree
    (:nicknames :cl-btree)
  (:use :common-lisp :alexandria)
  (:export #:btree-insert
	   #:btree-delete
	   #:btree-search
	   #:btree-map))

(in-package :btree)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;               Utility                  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *debug-output* *standard-output*)
(defparameter *debug-output* nil)

(defmacro debug-format  (fmt-str &rest rest)
  #+nil
  `(format *debug-output* ,fmt-str ,@rest))
	   


