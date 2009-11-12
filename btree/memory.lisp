(in-package :btree)

(declaim (optimize debug))

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
    :accessor node-keyvals)
   (values
    :initarg :values
    :accessor node-values)
   (children
    :initarg :children
    :accessor node-children)
   (parent
    :initarg :parent
    :accessor node-parent))
  (:documentation "btree stored in memory"))

(defmethod btree-make-node (btree &key parent &allow-other-keys)
  (make-instance 'memory-btree-node
		 :parent parent
		 :keyvals (make-array (1+ (btree-max-keys btree)) :fill-pointer 0 :adjustable nil)
		 :children (make-array (1+ (btree-max-keys btree)) :fill-pointer 0 :adjustable nil)))

(defmethod initialize-instance ((btree memory-btree)  &key &allow-other-keys)
  (call-next-method)
  (setf (btree-root btree) (btree-make-node btree :parent nil))
  btree)

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

(defmethod btree-node-parent ((tree memory-btree) (node memory-btree-node))
  (declare (ignore tree))
  (node-parent node))

(defun insert-into-array (array value position)
  (vector-push nil array)
  (replace array array :start2 position :start1 (1+ position))
  (setf (aref array position) value)
  array)

(defmethod btree-node-position-for-key ((tree memory-btree)
					(node memory-btree-node)
					key &key maxp &allow-other-keys)
  (let* ((test (btree-test tree))
	 (predicate #'(lambda (tree-key) (funcall test tree-key key)))
	 (position-of-key (1+ (or (position-if predicate
					       (node-keyvals node)
					       :from-end t
					       :key #'car)
				  -1))))
    position-of-key))

(defmethod btree-node-child-at-position  ((tree memory-btree) (node memory-btree-node) position)
  (aref (node-children node) position))

(defmethod btree-node-entry-at-position ((tree memory-btree)
					 (node memory-btree-node)
					 position)
  (let ((cons (aref (node-keyvals node) position)))
    (values (car cons) (cdr cons))))


(defmethod btree-replace-root ((tree memory-btree)
			       &key key value left-child right-child)
  (format t "Repalcing Root: ~A ~A~%" left-child right-child)
  
  (let ((root (btree-make-node tree :parent nil)))
    (setf (btree-root tree) root)
    (btree-node-insert-at-position tree root 0 key :value value
				   :right-child right-child  :left-child left-child)
    root))

(defmethod btree-node-insert-at-position  ((tree memory-btree)
					   (node memory-btree-node)
					   position key &key value left-child right-child &allow-other-keys)
  (assert (or (not (and left-child right-child))
	      (and (btree-node-rootp tree node) (= 0 (length (node-keyvals node))))))
  (insert-into-array (node-keyvals node) (cons key value) position)
  (when left-child
    (setf (node-parent left-child) node)
    (insert-into-array (node-children node) left-child position))
  (when right-child
    (setf (node-parent right-child) node)
    (insert-into-array (node-children node) right-child (1+ position)))
  tree)

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
  (let ((key-position (or key-position (btree-node-position-for-key tree node key)))
	(median-position (floor (/ (btree-max-keys tree) 2))))

    (btree-node-insert-at-position tree node key-position key
				   :value value :left-child left-child :right-child right-child)

    (multiple-value-bind (median-key median-value)
	(btree-node-entry-at-position tree node median-position)

      (let ((right (let ((r (btree-make-node tree)))
		     (replace (node-keyvals r) 
			      (node-keyvals node)
			      :start2 (1+ median-position))
		     (when (not (btree-node-leafp tree node))
		      ;; absorb the median's right child into the new right node
		       (replace (node-children r) 
				(node-children node)
				:start2 (1+ median-position)))
		     r))
	    (left (progn
		    (setf (fill-pointer (node-keyvals node)) median-position)
		    (when (not (btree-node-leafp tree node))
		      ;; absorb the median's left child into the new left node
		      (setf (fill-pointer (node-children node)) (1+ median-position)))
		    node)))
	(format t "Split returned ~A ~A ~A ~A~%"  left right median-key median-value)
	(values left right median-key median-value)))))
			       


(defmethod btree-node-insert (tree node key &key key-position value left-child right-child &allow-other-keys)
  (if (not (btree-node-fullp tree node))
      ;; IF the node is not full, simply insert the new key and value at the the right position
      (let ((position (btree-node-position-for-key tree node key)))
	(assert position)
	(btree-node-insert-at-position tree node position key
					:value value
					:left-child left-child :right-child right-child))
      ;; If the node is full, then we split the node and insert the median value in the
      ;; parent node, with left and right nodes according to the split.
      (multiple-value-bind (left-split-child right-split-child median-key median-value)
	  (btree-node-split tree node
			    :key key :key-position key-position :value value
			    :left-child left-child :right-child right-child)
	(if (not (btree-node-rootp tree node))
	    (btree-node-insert tree (btree-node-parent tree node) median-key
			       :value median-value
			       ;:left-child left-split-child
			       :right-child right-split-child)
	    ;; make new root
	    (btree-replace-root tree :key median-key :value median-value
				:left-child left-split-child :right-child right-split-child)))))

(defmethod btree-insert (tree key value &rest rest &key &allow-other-keys)
  "1. By searching the tree, find the leaf node where the new element should be added.
2. If the leaf node contains fewer than the maximum legal number of elements, there is room for one more. Insert the new element in the node, keeping the node's elements ordered.
3. Otherwise the leaf node is split into two nodes.
      1. A single median is chosen from among the leaf's elements and the new element.
      2. Values less than the median are put in the new left node and values greater than the median are put in the new right node, with the median acting as a separation value.
      3. That separation value is added to the node's parent, which may cause it to be split, and so on."
  (multiple-value-bind (node position)
      (apply #'btree-position-for-key tree key rest)
    (assert node)
    (assert position)
    (assert (btree-node-leafp tree node))
    (btree-node-insert tree node key :value value :key-position position)))

(defmethod btree-position-for-key (tree key &rest rest &key maxp &allow-other-keys)
  (declare (ignore maxp))
  (let* ((node (btree-root tree))
	 (position (apply #'btree-node-position-for-key tree node key rest)))
    (loop :until (btree-node-leafp tree node)
	  :do (setf node (btree-node-child-at-position tree node position)
		    position (apply #'btree-node-position-for-key tree node key rest)))
    (values node position)))
	 
(defmethod btree-search (tree key &rest rest &key &allow-other-keys)
  "Search is performed in the typical manner, analogous to that in a binary search tree. Starting at
the root, the tree is traversed top to bottom, choosing the child pointer whose separation values are
on either side of the value that is being searched."
  (multiple-value-bind (node position)
      (apply #'btree-position-for-key tree key rest)
    (btree-node-entry-at-position tree node position)))