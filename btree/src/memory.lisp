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
    :accessor node-children))

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

;; TODO add from-end key
(defun ordered-vector-insert-position (find-value vec &key (start 0) (end (length vec))
				       (key #'identity) (less-than #'<))
  "In the absense of maxp, finds the minimum position I in VEC at
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
		 (ordered-vector-insert-position find-value vec
						 :start start :end center
						 :key key :less-than less-than))
		(t			; find-value >= key-value
		 (ordered-vector-insert-position find-value vec
						 :start center :end end
						 :key key :less-than less-than))))))))

(defun ordered-vector-insert-position (find-value vec &key (start 0) (end (length vec))
				       (key #'identity) (less-than #'<))
  "In the absense of maxp, finds the minimum position I in VEC at
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


(defmethod btree-node-position-for-key ((tree memory-btree)
					(node memory-btree-node)
					key &key maxp &allow-other-keys)
  (ordered-vector-insert-position key (node-keyvals node) :less-than (btree-test tree) :key #'car)
  #+nil
  (let* ((test (btree-test tree))
	 (predicate #'(lambda (tree-key) (funcall test tree-key key)))
	 (position-of-key (1+ (or (position-if predicate
					       (node-keyvals node)
					       :from-end t
					       :key #'car)
				  -1))))
    position-of-key))

(defmethod btree-node-child-at-position  ((tree memory-btree) (node memory-btree-node) position)
  (assert (not (btree-node-leafp tree node)))
  (assert (< position (length (node-children node))))
  (assert (btree-nodep tree   (aref (node-children node) position)))
  (aref (node-children node) position))
    


(defmethod btree-node-entry-at-position ((tree memory-btree)
					 (node memory-btree-node)
					 position)
  (let ((cons (aref (node-keyvals node) position)))
    (values (car cons) (cdr cons))))


(defmethod btree-replace-root ((tree memory-btree)
			       &key key value left-child right-child)
  (debug-format "Replacing root: ~A ~A~%" left-child right-child)
  
  (let ((root (btree-make-node tree)))
    (setf (btree-root tree) root)
    (btree-node-insert-at-position tree root 0 key :value value
				   :right-child right-child  :left-child left-child)
    (debug-format "New root: ~A~%" root)
    root))

(defmethod btree-node-insert-at-position  ((tree memory-btree)
					   (node memory-btree-node)
					   position key &key value left-child right-child &allow-other-keys)
  (assert (or (not (and left-child right-child))
	      (and (btree-node-rootp tree node) (= 0 (length (node-keyvals node))))))
  (insert-into-array (node-keyvals node) (cons key value) position)
  (when left-child
    (insert-into-array (node-children node) left-child position))
  (when right-child
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



(defmethod btree-node-insert (tree node key &key key-position value left-child right-child parents &allow-other-keys)
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
	(sanity-check-btree tree)
	(if (not (btree-node-rootp tree node))
	    (btree-node-insert tree (first parents) median-key
			       :value median-value
			       ;:left-child left-split-child
			       :right-child right-split-child
			       :parents (rest parents))
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
  (multiple-value-bind (node position parents)
      (apply #'btree-position-for-key tree key rest)
    (assert node)
    (assert position)
    (assert (btree-node-leafp tree node))
    (btree-node-insert tree node key :value value :key-position position :parents parents)))

(defmethod btree-position-for-key (tree key &rest rest &key &allow-other-keys)
  (let* ((node (btree-root tree))
	 (position (apply #'btree-node-position-for-key tree node key rest))
	 (parent-nodes nil))
    (loop :until (btree-node-leafp tree node)
	  :do (progn
		(push node parent-nodes)
		(setf node (btree-node-child-at-position tree node position)
		      position (apply #'btree-node-position-for-key tree node key rest))))
    (values node position parent-nodes)))
	 
(defmethod btree-search (tree key &rest rest &key &allow-other-keys)
  "Search is performed in the typical manner, analogous to that in a binary search tree. Starting at
the root, the tree is traversed top to bottom, choosing the child pointer whose separation values are
on either side of the value that is being searched."
  (multiple-value-bind (node position)
      (apply #'btree-position-for-key tree key rest)
    (multiple-value-bind (k v)
	(btree-node-entry-at-position tree node position)
      (when (btree-value-equalp tree k key)
	(values k v t)
	(values nil nil nil)))))

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