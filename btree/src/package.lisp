(defpackage :btree
    (:nicknames :cl-btree)
  (:use :common-lisp :alexandria)
  (:export #:btree-insert
	   #:btree-search
	   #:btree-map))

(in-package :btree)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;               Utility                  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *debug-output* *standard-output*)
(defparameter *debug-output* nil)

(defmacro debug-format  (fmt-str &rest rest)
  `(format *debug-output* ,fmt-str ,@rest))
	   


;; upper bound by binary search.
(defun ordered-vector-insert-position-upper (find-value vec &key (start 0) (end (length vec))
				       (key #'identity) (less-than #'<))
  "In the absense of maxp, finds the maximum position I in VEC at
which FIND-VALUE is not less than (elt vec I), or else returns the
length of the vector."
  (declare (type (vector *) vec)
           (type fixnum start end)
	   (optimize (speed 3)))
  (if (= 0 (- end start))
      start
      (let* ((center (floor (+ start end) 2))
	     (value (aref vec center))
	     (key-value (funcall key value)))
	(if (= (- end start) 1)
	    (if (funcall less-than find-value key-value)
		start
		(1+ start))
	    (let ((find-value<key-value (funcall less-than find-value key-value)))
	      (cond
		(find-value<key-value
		 (ordered-vector-insert-position-upper find-value vec
						 :start start :end center
						 :key key :less-than less-than))
		(t			; find-value >= key-value
		 (ordered-vector-insert-position-upper find-value vec
						 :start center :end end
						 :key key :less-than less-than))))))))

(defun ordered-vector-insert-position (find-value vec &key (start 0) (end (length vec))
				       (key #'identity) (less-than #'<))
  "In the absense of maxp, finds the minimum position I in VEC at
which FIND-VALUE is not less than (elt vec I), or else returns the
length of the vector."
  (declare (type (vector *) vec)
           (type fixnum start end)
	   (type function less-than)
	   (optimize (speed 3)))
  (if (= 0 (- end start))
      start
      (let* ((center (floor (+ start end) 2))
	     (value (aref vec center))
	     (key-value (funcall key value)))
	(if (= (- end start) 1)
	    (if (funcall less-than find-value key-value)
		start
		(1+ start))
	    (let ((key-value<find-value (funcall less-than key-value find-value)))
	      (cond
		(key-value<find-value
		 (ordered-vector-insert-position find-value vec
						 :start center :end end
						 :key key :less-than less-than))
		(t
		 (ordered-vector-insert-position find-value vec
						 :start start :end center
						 :key key :less-than less-than))))))))