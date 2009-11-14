(in-package :btree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Public Generics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric btree-insert (tree key value &key &allow-other-keys)
  (:documentation "Inserts the given key and value into the btree."))

(defgeneric btree-delete (tree key &key &allow-other-keys)
  (:documentation "Deletes the given key from the btree."))

(defgeneric btree-search (tree key &key &allow-other-keys)
  (:documentation "Returns 3 values: the value and the ky of the entry found, and T if one
was actually found (in case nil nil are the key and value)."))

(defgeneric btree-map (tree map-fn &key start end value from-end &allow-other-keys)
  (:documentation "Returns 3 values: the key and value of of the entry found, and T if one
was actually found (in case nil nil are the key and value)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; BTree Protocol Generics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; BTree interface
(defgeneric btree-max-keys (btree)
  (:documentation "The maximum number of keys allowed in each node."))

(defgeneric btree-min-keys (btree)
  (:documentation "The minimum number of keys allowed in each node.

DEFAULT PROVIDED based on BTREE-MAX-KEYS."))

(defgeneric btree-root (tree)
  (:documentation "Returns the root node of a btree"))

(defgeneric btree-replace-root (tree &key key value left-child right-child)
  (:documentation "Called in the process of insertion if it is necessary to split the
root."))

(defgeneric btree-value-equalp (tree value test-value)
  (:documentation "Returns non-nil iff value equals test-value
according to the comparison used by the btree."))

(defgeneric btree-position-for-key (tree key &key testp &allow-other-keys)
  (:documentation "Given a btree and a key, returns 2 values: 
1. The node into which the key either is found or could be added if it does not exist.
2. A valid position that indicates where in the given node that key may be found
or could be inserted.

Even if an node is full both values should be non-null.  Duplicate entries are allowed
and if MAXP is non-null, then the maximum position at which the key may be INSERTED is
returned (so 1 past the last value that matches key)."))

(defgeneric btree-insert-at-position (tree position key value &key left-child right-child &allow-other-keys)
  (:documentation "Inserts the given KEY/VALUE pair into the given TREE at the given POSITION."))

(defgeneric btree-delete-at-position (tree position &key &allow-other-keys)
  (:documentation "Deletes the KEY/VALUE pair in the given TREE at the given POSITION."))

(defgeneric btree-delete-from-minimally-filled-leaf (tree position &key &allow-other-keys)
  (:documentation "Deletes the key/value pair at the given position in the btree, when 
the given positioin designates a leaf node with exactly the minimum number of required key/vals.

Recruits a key/value pair from adjacent nodes, or merges the node with an adjacent
minimally-filled node if need be."))

(defgeneric btree-delete-from-internal-node (tree position &key &allow-other-keys)
  (:documentation "Deletes the key/value at the given position, when the given
position designates an internal node in the btree (i.e. one with children)."))

;;;; Node interface
(defgeneric btree-nodep (tree node)
  (:documentation "Returns T if the given thing is a node in the given
tree.  If T then that node must conform to this protocol."))

(defgeneric btree-node-leafp (tree node)
  (:documentation "Returns T if the given node is a leaf node."))

(defgeneric btree-node-rootp (tree node)
  (:documentation "Returns T if the given node is a leaf node."))

(defgeneric btree-node-fullp (tree node)
  (:documentation "Returns T if the node is full.

DEFAULT PROVIDED based on BTREE-NODE-KEYCOUNT and BTREE-MAX-KEYS."))

(defgeneric btree-node-keycount (tree node)
  (:documentation "Returns the number of key/value pairs in a particular node."))

(defgeneric btree-node-offset-for-key (tree node key &key maxp &allow-other-keys)
  (:documentation "Given a btree and a key, returns a number in [0,
MAX-KEYS] that indicates where in the given node that key may be found
or could be inserted.  This does not require that the key at that
offset in the node is equal to the provided key, but merely that an
insertion at this offset in the node would preserve the order of the
node's key/value pairs.  The offset does not take into account child
or parent nodes."))

(defgeneric btree-node-child-at-offset (tree node offset)
  (:documentation "If leftp is T, returns the child node to the left of the OFFSET-th
element stored in this node.  If leftp is NIL, returns the right child."))

(defgeneric btree-node-entry-at-offset (tree node offset)
  (:documentation "Returns 2 values: the key and the value at the given offset in
the given tree and node, as returned by BTREE-POSITION-FOR-KEY"))

(defgeneric btree-node-insert-unfilled  (tree node offset key value &key left-child right-child &allow-other-keys)
  (:documentation "Inserts the given key and value into the node at the given offset.  This
is only called on a node that is under the maximum size and this will avoid having to split
and invoke subsequent operations on the parents."))

(defgeneric btree-node-append-key-values-from-node (tree node other-node)
  (:documentation "Moves all the key/value pairs from OTHER-NODE into NODE.  This happens
when the OTHER-NODE is being discarded in a delete due to underflow. "))

(defgeneric btree-node-replace-key-value  (tree node offset key value &key &allow-other-keys)
  (:documentation "Replaces a given key/value pair in a node with the supplied
key and value.  This is used during deletion and possibly during insertion in
the provided generic implementation.  It does not alter the children or anything
else."))

(defgeneric btree-node-split (tree node &key key value left-child right-child)
  (:documentation "When a given a node, splits it in half.  The left half of the keys/values
are kept in this node, and a new node is created with the right half.  Returns the following
values:
1.  Left node.
2.  Right node.
3.  Median key.
4.  Median value."))

(defgeneric btree-node-delete-from-sufficiently-filled-leaf (tree node offset &key &allow-other-keys)
  (:documentation "Deletes the key/value pair at offset from the leaf node NODE
when NODE has more than the minimum number of key/value pairs."))

(defgeneric btree-node-delete-from-leaf (tree node offset &key &allow-other-keys)
  (:documentation "Deletes the key/value pair at offset from the leaf node NODE.
Allows for underflow (i.e. minimum number of key/value pairs in leaf"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Generic Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (optimize (debug 3)))

(defmethod btree-min-keys (btree)
  (floor (btree-max-keys btree) 2))

(defclass btree-position ()
  ((node-offset :initarg :node-offset :accessor position-node-offset
		:type fixnum)
   (node        :initarg :node :accessor position-node)
   (parent      :initarg :parent :initform nil :accessor position-parent
		:documentation "Parent position object or NIL if node is the root.")))

(defmethod print-object ((o btree-position) stream)
  (with-accessors ((no position-node-offset) (node position-node) (parent position-parent))
      o
    (print-unreadable-object (o stream :type t)
      (format stream "~S[~A] ~A parent" node no (if parent "with" "without")))))

(defmethod btree-search (tree key &rest rest &key &allow-other-keys)
  "Search is performed in the typical manner, analogous to that in a
binary search tree. Starting at the root, the tree is traversed top to
bottom, choosing the child pointer whose separation values are on
either side of the value that is being searched."
  (multiple-value-bind (position k v)
      (apply 'btree-search* tree key rest)
    (if position
	(values k v t)
	(values nil nil nil))))

(defun btree-search* (tree key &rest rest)
  "Like btree-search but returns the position, key, and value of the BTree entry matching
key."
  (let ((position (when-let (pos (apply #'btree-position-for-key tree key :testp t rest))
		    (when (< (position-node-offset pos)
			     (btree-node-keycount tree (position-node pos)))
		      pos))))
    (if position
	(multiple-value-bind (k v)
	    (btree-node-entry-at-offset tree (position-node position) (position-node-offset position))
	  (if (btree-value-equalp tree k key)
	      (values position v k t)
	      (values nil nil nil)))
	(values nil nil nil))))
  

(defmethod btree-node-fullp (tree node)
  (= (btree-max-keys tree) (btree-node-keycount tree node)))

(defun key-at-position-matches (tree position key)
  (when (< (position-node-offset position)
	   (btree-node-keycount tree (position-node position)))
    (multiple-value-bind (k v)
	(btree-node-entry-at-offset tree (position-node position) (position-node-offset position))
      (declare (ignore v))
      (btree-value-equalp tree k key))))

(defmethod btree-position-for-key (tree key &key testp &allow-other-keys)
  (flet ((position-of-key-in-node (node parent-position)
	   (make-instance 'btree-position
			  :node node
			  :node-offset (btree-node-offset-for-key tree node key)
			  :parent parent-position)))
    (let* ((node (btree-root tree))
	   (position (position-of-key-in-node node nil)))
      (loop :until (or (btree-node-leafp tree node)
		       (and testp (key-at-position-matches tree position key)))
	    :do (progn
		  (setf node (btree-node-child-at-offset tree node (position-node-offset position))
			position (position-of-key-in-node node position))))
      position)))

(defmethod btree-insert (tree key value &rest rest &key &allow-other-keys)
  "1. By searching the tree, find the leaf node where the new element should be added.
2. If the leaf node contains fewer than the maximum legal number of elements, there is room for one more. Insert the new element in the node, keeping the node's elements ordered.
3. Otherwise the leaf node is split into two nodes.
      1. A single median is chosen from among the leaf's elements and the new element.
      2. Values less than the median are put in the new left node and values greater than the median are put in the new right node, with the median acting as a separation value.
      3. That separation value is added to the node's parent, which may cause it to be split, and so on."
  (let ((position (apply #'btree-position-for-key tree key rest)))
    (assert position)
    (assert (position-node position))
    (assert (btree-node-leafp tree (position-node position)))
    (btree-insert-at-position tree position key value)))

(defmethod btree-delete (tree key &rest rest &key &allow-other-keys)
  (multiple-value-bind (position k v)
      (apply 'btree-search* tree key rest)
    (declare (ignore k v))
    (if position
	(btree-delete-at-position tree position)
	(values nil nil nil))))

(defmethod btree-insert-at-position (tree position key value &key left-child right-child &allow-other-keys)
  (let ((node (position-node position)))
    (if (not (btree-node-fullp tree node))
	;; IF the node is not full, simply insert the new key and value at the the right position
	(let ((offset (position-node-offset position)))
	  (assert offset)
	  (btree-node-insert-unfilled tree node offset key value
				      :left-child left-child :right-child right-child))
	;; If the node is full, then we split the node and insert the median value in the
	;; parent node, with left and right nodes according to the split.
	(multiple-value-bind (left-split-child right-split-child median-key median-value)
	    (btree-node-split tree node
			      :key key :value value :key-position (position-node-offset position)
			      :left-child left-child :right-child right-child)
	  ;; (sanity-check-btree tree)
	  (if (not (btree-node-rootp tree node))
	      (btree-insert-at-position tree (position-parent position)
					median-key median-value
					;:left-child left-split-child
					:right-child right-split-child)
	      ;; make new root
	      (btree-replace-root tree :key median-key :value median-value
				  :left-child left-split-child :right-child right-split-child))))))

(defmethod btree-delete-at-position (tree position &key &allow-other-keys)
  (let* ((node (position-node position))
	 (leafp (btree-node-leafp tree node)))
    (cond
      ((and leafp (or (> (btree-node-keycount tree node) (btree-min-keys tree))
		      (btree-node-rootp tree node)))
	;; if it's a leaf and it has more than the minimum number of elements
       (btree-node-delete-leaf-sufficiently-filled tree node (position-node-offset position)))
      
      (leafp
       ;; min-filled leaf
       (assert (= (btree-node-keycount tree node) (btree-min-keys tree)))
       (btree-delete-from-minimally-filled-leaf tree position))

      (t
       ;; internal delete
       ;;  from incrementing or decrementing position by one into position's
       ;; place, deleting it from the leaf from which it comes
       (btree-delete-from-internal-node tree position)))))

(defun btree-mov-key-value (tree source-position destination-position source-action destination-action)
  "Takes the key/value from SOURCE-POSITION and copies it into DESTINATION-POSITION.

If SOURCE-ACTION is :delete, the key/value at SOURCE-POSITION is deleted before returning,
otherwise no modification of the source node's key/values occurs.

If DESTINATION-ACTION is replace, btree-node-replace-key-value is used to replace
the key/value described by DESTINATION-POSITION.  If DESTINATION-ACTIO is :insert-unfilled,
then btree-node-insert-unfilled is used to insert the key/value into the given postion."
  (declare (type (member :delete-from-leaf :delete-keyval-and-right-child nil) source-action)
	   (type (member :insert-unfilled :replace) destination-action))

  (multiple-value-bind (key value)
      (btree-node-entry-at-offset tree
				  (position-node source-position)
				  (position-node-offset source-position))
    (case source-action
      (:delete-keyval-and-right-child
	 (error "No implementation for deleting right child."))
      (:delete-from-leaf
	 (btree-node-delete-from-sufficiently-filled-leaf tree
							  (position-node source-position)
							  (position-node-offset source-position)))
      (t))

    (case destination-action
      (:insert-unfilled
	 (btree-node-insert-unfilled tree 
				     (position-node destination-position)
				     (position-node-offset destination-position)
				     key value))
      (:replace
	 (btree-node-replace-key-value tree 
				       (position-node destination-position)
				       (position-node-offset destination-position)
				       key value)))))

(defun btree-compute-underflow-action (tree deleted-position)
  "Computes how to deal with underflow encountered in a leaf node during deletion from
a BTree.  POSITION designates the position of a child node in some parent node, where
the child node is a leaf and has just encountered underflow from deleting a key/value
pair.

This function determines the appropriate action to take next: either stealing a key/value
pair from a sibling, or merging the node with one of its siblings.

The returns values for this function are as follows:

1.  ACTION.
    :MERGE when the leaf node at POSITION should be merged with a child, or
    :SHIFT-ONE when the leaf node should absorb a key/value pair from a sibling

2.  PARENT-KEYVAL-POSITION.
    A position that indicates key/value position in the PARENT node in between the
    sibling node and the node indicated by POSITION

3.  DESTINATION-KEYVAL-POSITION
    A position that indicates key/value position in the DESTINATION node.  In the case
    where 1 is either :MERGE or :SHIFT-ONE, this value indicates the position into which
    the PARENT key/value pair (designated by return value 2) needs to be inserted.

    If ACTION is :MERGE, this position designates an offset into the surviving node
    from the deletion.  It will always be the left sibling that remains.  Key/value
    pairs from the right node will need to be appended to the surviving node.  
    

4.  If ACTION is :SHIFT-ONE, this value is SIBLING-KEYVAL-POSITION, the position from
    which a key/value pair needs to be shifted into PARENT-KEYVAL-POSITION after
    the parent's key/value pair is shifted into the destination.

    When ACTION is :MERGE, this value is not a position but the sibling node itself.

"
  ;; Everything here is named in reference to the underflowed node (e.g. parent is
  ;; the parent of the underflowed node

  (let* ((underflowed-node (position-node deleted-position))
	 (parent-position (position-parent deleted-position))
	 (parent (position-node parent-position)) ;; parent node of underflowed node
	 (offset-in-parent (position-node-offset parent-position))
	 ;; Decide on a sibling
  	 (sibling-leftp (not (= 0 offset-in-parent))) ;; could make a wiser decision, but this avoid I/O
	 (sibling-offset-in-parent  (+ offset-in-parent (if sibling-leftp -1 1)))
	 (sibling-node (btree-node-child-at-offset tree parent sibling-offset-in-parent))
	 (left-node  (if sibling-leftp       sibling-node underflowed-node))
	 (right-node (if (not sibling-leftp) sibling-node underflowed-node))
	 (sibling-keycount (btree-node-keycount tree sibling-node))
	 ;; Determine what action to take based on whether the sibling has extra key/vals
	 (action (if (= sibling-keycount  (btree-min-keys tree))
		     :merge
		     :shift-one))
	 ;; The Parent key/value will 
	 (parent-keyval-position
	  (make-instance 'btree-position
			 :node parent
			 :node-offset (if sibling-leftp sibling-offset-in-parent offset-in-parent)
			 :parent (position-parent parent-position)))
	 (destination-keyval-position
	  (if (eql :shift-one action)
	      ;; destination of a shift action is always the underflowed node
	      (make-instance 'btree-position
			     :node underflowed-node
			     :node-offset (if sibling-leftp 0 (btree-node-keycount tree underflowed-node))
			     :parent parent-position)
	      ;; destination of a merge action is always the left node
	      (make-instance 'btree-position
			     :node left-node
			     :node-offset (btree-node-keycount tree left-node)
			     :parent parent-keyval-position)))
	 (sibling-info
	  (if (eql :shift-one action)
	      ;; sibling-info of a shift action is the position in the sibling node
	      ;; from which we move a value into the parent's key/value position
	      (make-instance 'btree-position
			     :node sibling-node
			     :node-offset (if (not sibling-leftp) 0 (btree-node-keycount tree sibling-node))
			     :parent (make-instance 'btree-position
						    :node parent
						    :node-offset sibling-offset-in-parent
						    :parent (position-parent parent-position)))
	      ;; sibling-info of a merge action is the right node (the discarded node)
	      right-node)))
    (values action parent-keyval-position destination-keyval-position sibling-info)))
	 
(defmethod  btree-delete-from-minimally-filled-leaf (tree position &key &allow-other-keys)
  ;; Find the closest sibling to the node at which we are deleting the value
  ;; If that node has MORE than the minimum number of elements, we shift one of them
  ;; up to the parent node and shift the parent node's
  
  
  (let ((this-node (position-node position))
	(this-node-offset (position-node-offset position)))
    ;; 1. Delete the key/value pair from the leaf
    (btree-node-delete-from-leaf tree this-node this-node-offset)

    ;; Case 1: we do not need to merge sibling nodes
    (multiple-value-bind (action
			  parent-keyval-position
			  destination-keyval-position
			  sibling-info)
	(btree-compute-underflow-action tree position)
      (declare (type (member :merge :shift-one) action))
      (case action
	(:shift-one
	   (assert (eql this-node (position-node destination-keyval-position)))
	   (assert (eql (position-node (position-parent position))
			(position-node parent-keyval-position)))
	   (let ((sibling-keyval-position sibling-info))
	     ;; move parent key/value into this node at the given position
	     (btree-mov-key-value tree
				  parent-keyval-position
				  destination-keyval-position
				  nil :insert-unfilled)
	     ;; move the sibling key/value into the key/value position we just replaced
	     (btree-mov-key-value tree
				  sibling-keyval-position
				  parent-keyval-position
				  :delete-from-leaf :replace)))
	(:merge
	   (let ((sibling-node sibling-info)
		 (surviving-node (position-node destination-keyval-position)))
	     (assert (eql (position-node (position-parent position))
			  (position-node parent-keyval-position)))
	     ;; move parent key/value into destination node at the given position
	     (btree-mov-key-value tree
				  parent-keyval-position
				  destination-keyval-position
				  :delete-keyval-and-right-child :insert-unfilled)
	     ;; append the key/values from right node to the left node
	     (btree-node-append-key-values-from-node tree
						     surviving-node
						     sibling-node)))))))
						     
						     
(defmethod btree-delete-from-internal-node (tree position &key &allow-other-keys)
  ;; move key/value pair from child (either the first key/value in the RIGHT child
  ;; or the last key-value in the LEFT child) into this node and then delete it from
  ;; the child.

  ;; Snatch a value from the left child and switch it out for our key-value
  (let* ((this-node (position-node position))
	 (this-node-offset  (position-node-offset position))
	 (left-child (btree-node-child-at-offset tree this-node this-node-offset))
	 (offset-in-child (1- (btree-node-keycount tree left-child)))
	 (position-in-child (make-instance 'btree-position
					   :node left-child
					   :node-offset offset-in-child
					   :parent position)))
    ;; replace
    (multiple-value-bind (child-key child-value)
	(btree-node-entry-at-offset tree left-child offset-in-child)
      (btree-node-replace-key-value tree this-node this-node-offset
				    child-key child-value))
    ;; delete from the child
    (btree-delete-at-position tree position-in-child)))

(defmethod btree-node-append-key-values-from-node (tree node other-node)
  (assert (btree-node-leafp tree node))
  (assert (btree-node-leafp tree other-node))
  (dotimes (i (btree-node-keycount tree other-node))
    (multiple-value-bind (key value)
	(btree-node-entry-at-offset tree other-node i)
      ;; insert key/value pair at the end
      (btree-node-insert-unfilled tree node (btree-node-keycount tree node)
				  key value))))