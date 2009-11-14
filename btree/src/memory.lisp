(in-package :btree)

(declaim (optimize (debug 3)))

(defclass memory-btree ()
  ((root-node
    :initform nil :initarg :root
    :accessor btree-root)
   (max-keys
    :initform 2 :initarg :max-keys
    :accessor btree-max-keys)
   (test
    :initform #'< :initarg :predicate
    :accessor btree-test))
  (:documentation "btree stored in memory"))

(defclass memory-btree-node ()
  ((keyvals
    :initarg :keyvals
    :accessor node-keyvals
    :documentation "Array of key-value conses.  Capacity is MAX-KEYS+1")
   (children
    :initarg :children
    :accessor node-children
    :documentation "Array of child nodes.  Capicity is MAX-KEYS+2.
Leafs have 0-length child array."))
  (:documentation "btree stored in memory"))

(defmethod print-object ((obj memory-btree-node) s)
  (print-unreadable-object (obj s :type nil :identity nil)
    (format s "NODE keyvals: ~S children: ~S"
	    (node-keyvals obj)
	    (node-children obj))))

(defmethod btree-make-node (btree &key &allow-other-keys)
  (make-instance 'memory-btree-node
		 :keyvals (make-array (1+ (btree-max-keys btree)) :fill-pointer 0 :adjustable nil)
		 :children (make-array (+ 2 (btree-max-keys btree)) :fill-pointer 0 :adjustable nil)))

(defmethod initialize-instance ((btree memory-btree)  &key &allow-other-keys)
  (call-next-method)
  (setf (btree-root btree) (btree-make-node btree))
  btree)

(defmethod btree-node-keycount ((tree memory-btree) (node memory-btree-node))
  (or (when-let (keyvals (node-keyvals node)) (length keyvals))
      0))

(defmethod btree-value-equalp ((tree memory-btree) value test-value)
  (let ((less (btree-test tree)))
    (and (not (funcall less value test-value))
	 (not (funcall less test-value value)))))

(defmethod btree-nodep ((tree memory-btree) (node memory-btree-node))
  t)

(defmethod btree-node-leafp ((tree memory-btree) (node memory-btree-node))
  (= 0 (length (node-children node))))

(defmethod btree-node-rootp ((tree memory-btree) (node memory-btree-node))
  "EQL test is default."
  (eql (btree-root tree) node))

(defmethod btree-node-fullp ((tree memory-btree) (node memory-btree-node))
  (= (btree-max-keys tree)
     (length (node-keyvals node))))

(defun insert-into-array (array value position)
  (vector-push nil array)
  (replace array array :start2 position :start1 (1+ position))
  (setf (aref array position) value)
  array)

(defmethod btree-node-offset-for-key ((tree memory-btree)
					(node memory-btree-node)
					key &key maxp &allow-other-keys)
  ;; the binary search version
  (ordered-vector-insert-position key (node-keyvals node) :less-than (btree-test tree) :key #'car)
  ;; the linear search version
  #+nil
  (let* ((test (btree-test tree))
	 (predicate #'(lambda (tree-key) (funcall test tree-key key)))
	 (position-of-key (1+ (or (position-if predicate
					       (node-keyvals node)
					       :from-end t
					       :key #'car)
				  -1))))
    position-of-key))

(defmethod btree-node-child-at-offset  ((tree memory-btree) (node memory-btree-node) position)
  (assert (not (btree-node-leafp tree node)))
  (assert (< position (length (node-children node))))
  (assert (btree-nodep tree   (aref (node-children node) position)))
  (aref (node-children node) position))
    


(defmethod btree-node-entry-at-offset ((tree memory-btree)
					 (node memory-btree-node)
					 position)
  (assert (< position (btree-node-keycount tree node)))
  (let ((cons (aref (node-keyvals node) position)))
    (values (car cons) (cdr cons))))


(defmethod btree-replace-root ((tree memory-btree)
			       &key key value left-child right-child)
  (debug-format "Replacing root: ~A ~A~%" left-child right-child)
  
  (let ((root (btree-make-node tree)))
    (setf (btree-root tree) root)
    (btree-node-insert-unfilled tree root 0 key value
				   :right-child right-child  :left-child left-child)
    (debug-format "New root: ~A~%" root)
    root))

(defmethod btree-node-insert-unfilled  ((tree memory-btree)
					   (node memory-btree-node)
					   offset key value &key left-child right-child &allow-other-keys)
  (assert (or (not (and left-child right-child))
	      (and (btree-node-rootp tree node) (= 0 (length (node-keyvals node))))))
  (insert-into-array (node-keyvals node) (cons key value) offset)
  (when left-child
    (insert-into-array (node-children node) left-child offset))
  (when right-child
    (insert-into-array (node-children node) right-child (1+ offset)))
  tree)

(defmethod btree-node-replace-key-value  ((tree memory-btree) (node memory-btree-node)
					  offset key value &key &allow-other-keys)
  (setf (aref (node-keyvals node) offset) (cons key value)))

(defmethod btree-node-delete-from-sufficiently-filled-leaf ((tree memory-btree)
							    (node memory-btree-node)
							    offset &key &allow-other-keys)
  (assert (> (btree-node-keycount tree node) (btree-min-keys tree)))
  (btree-node-delete-from-leaf tree node offset))

(defmethod btree-node-delete-from-leaf ((tree memory-btree)
					(node memory-btree-node)
					offset &key &allow-other-keys)
  (with-accessors ((keyvals node-keyvals))
      node
    (setf keyvals (delete-if (constantly t) keyvals :start offset :count 1))))


(defmethod btree-node-split ((tree memory-btree)
			     (node memory-btree-node)
			     &key key key-position value left-child right-child)
  "When a given a node, splits it in half.  The left half of the keys/values
are kept in this node, and a new node is created with the right half.  Returns the following
values:
1.  Left node.
2.  Right node.
3.  Median key.
4.  Median value."
  (let ((key-position (or key-position (btree-node-offset-for-key tree node key)))
	(median-position (floor (/ (btree-max-keys tree) 2))))

    (btree-node-insert-unfilled tree node key-position key value
				   :left-child left-child :right-child right-child)

    (multiple-value-bind (median-key median-value)
	(btree-node-entry-at-offset tree node median-position)

      (let ((right (let ((r (btree-make-node tree)))
		     ;(debug-format "Mapping ~A into right keyvals~%" (node-keyvals node))
		     (let ((num-keyvals (- (length (node-keyvals node)) (1+ median-position))))
		       (setf (fill-pointer (node-keyvals r)) num-keyvals)
		       (replace (node-keyvals r) 
				(node-keyvals node)
				:start2 (1+ median-position))
		     ;(debug-format "Mapped into ~A~%" (node-keyvals r))
		       (when (not (btree-node-leafp tree node))
			 ;; absorb the median's right child into the new right node
			 (setf (fill-pointer (node-children r)) (1+ num-keyvals))
			 (replace (node-children r) 
				  (node-children node)
				  :start2 (1+ median-position))))
		     r))
	    (left (progn
		    (setf (fill-pointer (node-keyvals node)) median-position)
		    (when (not (btree-node-leafp tree node))
		      ;; absorb the median's left child into the new left node
		      (setf (fill-pointer (node-children node)) (1+ median-position)))
		    node)))
	(assert (sanity-check-node left))
	(assert (sanity-check-node right))
	(debug-format "Split returned ~A ~A ~A ~A~%"  left right median-key median-value)
	(values left right median-key median-value)))))
      
(defun sanity-check-node (node)
  (assert (or (= 0 (length (node-children node)))
	      (> (length (node-children node))
		 (length (node-keyvals node)))))
  t)

(defun sanity-check-btree (tree)
  (declare (optimize (debug 3))))

(defun map-node-and-children (tree node fn)
  (flet ((keyval-fn (keyval)
	   (funcall fn (car keyval) (cdr keyval)))
	 (recurse (child) (map-node-and-children tree child fn))) 
    (if (btree-node-leafp tree node)
	(map nil #'keyval-fn (node-keyvals node))
	(map-alternating #'recurse #'keyval-fn
			 (node-children node) (node-keyvals node)))))

(defun map-alternating (fn1 fn2 seq1 seq2)
  "Calls FN1 with the first element of seq1, then calls FN2 with the
first element of seq1, then calls fn1 with the second element of seq2,
..."
  ;; implement it
  (loop :for i :from 0 :upto (1- (length seq1))
	:do (funcall fn1 (elt seq1 i))
	:unless (>= i (length seq2))
	:do (funcall fn2 (elt seq2 i))))

(defmethod btree-map ((tree memory-btree) map-fn &key start end value from-end &allow-other-keys)
  (map-node-and-children tree (btree-root tree) map-fn))