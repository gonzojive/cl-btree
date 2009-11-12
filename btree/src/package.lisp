(defpackage :btree
    (:nicknames :cl-btree)
  (:use :common-lisp :alexandria)
  (:export #:btree-insert
	   #:btree-search
	   #:btree-map))

(in-package :btree)

(defparameter *debug-output* *standard-output*)
(defparameter *debug-output* nil)

(defmacro debug-format  (fmt-str &rest rest)
  `(format *debug-output* ,fmt-str ,@rest))
	   
   