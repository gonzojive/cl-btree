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

(defgeneric btree-replace-root (tree &key node key value left-child right-child)
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

(defgeneric btree-delete-from-minimally-filled-node (tree position &key &allow-other-keys)
  (:documentation "Deletes the key/value pair at the given position in the btree, when 
the given positioin designates a node with exactly the minimum number of required key/vals.

Recruits a key/value pair from sibling nodes, or merges the node with a sibling
minimally-filled node if need be."))

(defgeneric btree-delete-from-internal-node (tree position &key &allow-other-keys)
  (:documentation "Deletes the key/value at the given position, when the given
position designates an internal node in the btree (i.e. one with children)."))

(defgeneric btree-position-successor (tree position &key &allow-other-keys)
  (:documentation "Returns a position that points to the next key/value pair in the
tree."))

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

(defgeneric btree-node-insert-child  (tree node offset child-node &key &allow-other-keys)
  (:documentation "Inserts the child node at offset into node."))

(defgeneric btree-join-nodes (tree node other-node)
  (:documentation "Moves all the key/value pairs and, in the case of an internal node, children,
from OTHER-NODE into NODE.  This happens when the OTHER-NODE is being discarded in a
delete due to underflow, and after the separation value from the parent node has
been moved down into NODE.  Thus, all children need to be moved because NODE now has
its own children only and not an extra child from the parent."))

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

(defgeneric btree-node-delete-from-node (tree node offset &key child-to-delete &allow-other-keys)
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
	 (leafp (btree-node-leafp tree node))
	 (minimally-filledp (= (btree-node-keycount tree node) (btree-min-keys tree))))
    (cond
      ((and leafp (or (not minimally-filledp)
		      (btree-node-rootp tree node)))
	;; if it's a leaf and it has more than the minimum number of elements
       (btree-node-delete-leaf-sufficiently-filled tree node (position-node-offset position)))
      
      (leafp
       ;; min-filled leaf
       (assert (= (btree-node-keycount tree node) (btree-min-keys tree)))
       (btree-delete-from-minimally-filled-node tree position))

      (t
       ;; internal delete
       ;;  from incrementing or decrementing position by one into position's
       ;; place, deleting it from the leaf from which it comes
       (btree-delete-from-internal-node tree position)))))

(defun btree-mov-child (tree source-position destination-position source-action destination-action)
  "Copies the child node from SOURCE-POSITION to DESTINATION-POSITION, taking actions on the
source and destination positions if specified.

If SOURCE-ACTION is :delete, the child is deleted from the source node SOURCE-POSITION.
Otherwise, no modification of the source node's key/values or children occurs.

DESTINATION-ACTION should always be :insert
"
  (declare (type (member :delete nil) source-action)
	   (type (member :insert) destination-action))

  (let ((source-node 	  (position-node source-position))
	(source-offset	  (position-node-offset source-position)))

  (let ((child (btree-node-child-at-offset tree source-node source-offset)))
    (case source-action
      (:delete (btree-node-delete-child tree source-node source-offset))
      (t))

    (case destination-action
      (:insert
	 (btree-node-insert-child tree
				  (position-node destination-position)
				  (position-node-offset destination-position)
				  child))))))

(defun btree-mov (tree source-position destination-position source-action destination-action
		  children)
  "Copies the the key/value (and maybe children) from SOURCE-POSITION to
DESTINATION-POSITION, taking actions on the source and destination positions if specified.

If SOURCE-ACTION is :delete, the key/value and potentillay children at SOURCE-POSITION
are deleted using btree-node-delete-from-node.  Otherwise, no modification of the source
node's key/values or children occurs.

If DESTINATION-ACTION is :replace-key-value, btree-node-replace-key-value is used to replace
the key/value described by DESTINATION-POSITION.
If DESTINATION-ACTION is :insert, then btree-node-insert-unfilled is used to insert
the key/value into the given postion.  If CHILDREN is supplied, then either the left, right,
or both children of SOURCE-POSITION (in case of :left, :right, and :both respectively) are
inserted into the destination position.
"
  (declare (type (member :delete nil) source-action)
	   (type (member :insert :replace-key-value) destination-action)
	   (type (member :left :right :both nil) children))

  (let ((source-node 	  (position-node source-position))
	(source-offset	  (position-node-offset source-position)))

  (multiple-value-bind (key value)
      (btree-node-entry-at-offset tree source-node source-offset)
    (let ((left-child (when (member children '(:left :both))
			(btree-node-child-at-offset tree source-node source-offset)))
	  (right-child (when (member children '(:right :both))
			 (btree-node-child-at-offset tree source-node (1+ source-offset)))))
      
    (case source-action
      (:delete
	 (btree-node-delete-from-node tree
				      source-node
				      source-offset
				      :child-to-delete children))
      (t))

    (case destination-action
      (:insert
	 (btree-node-insert-unfilled tree 
				     (position-node destination-position)
				     (position-node-offset destination-position)
				     key value
				     :left-child left-child
				     :right-child right-child))
      (:replace-key-value
	 (assert (null children))
	 (btree-node-replace-key-value tree 
				       (position-node destination-position)
				       (position-node-offset destination-position)
				       key value)))))))

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

5.  If ACTION is :SHIFT-ONE, this value is SIBLING-CHILD-POSITION, the position from
    which a child nodes needs to be shifted into DESTINATION-CHILD-POSITION.

6.  If ACTION is :SHIFT-ONE, this value is DESTINATION-CHILD-POSITION, the position to
    which a child node needs to be shifted into from the sibling


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
			     :node-offset (if (not sibling-leftp) 0 (1- (btree-node-keycount tree sibling-node)))
			     :parent (make-instance 'btree-position
						    :node parent
						    :node-offset sibling-offset-in-parent
						    :parent (position-parent parent-position)))
	      ;; sibling-info of a merge action is the right node (the discarded node)
	      right-node))
	 (sibling-child-position
	  (when (eql :shift-one action)
	    ;; sibling-info of a shift action is the position in the sibling node
	    ;; from which we move a value into the parent's key/value position
	    (make-instance 'btree-position
			   :node sibling-node
			   :node-offset (if (not sibling-leftp) 0 (btree-node-keycount tree sibling-node))
			   :parent (position-parent sibling-info))))
	 (destination-child-position
	  (when (eql :shift-one action)
	      ;; sibling-info of a shift action is the position in the sibling node
	      ;; from which we move a value into the parent's key/value position
	    (make-instance 'btree-position
			   :node underflowed-node
			   :node-offset (if sibling-leftp 0 (1+ (btree-node-keycount tree underflowed-node)))
			   :parent parent-position))))
    (values action
	    parent-keyval-position
	    destination-keyval-position
	    sibling-info
	    sibling-child-position
	    destination-child-position)))
	 
(defmethod  btree-delete-from-minimally-filled-node (tree position &key child-to-delete &allow-other-keys)
  (declare (type (member :left :right nil) child-to-delete))
  ;; Find the closest sibling to the node at which we are deleting the value
  ;; If that node has MORE than the minimum number of elements, we shift one of them
  ;; up to the parent node and shift the parent node's
  
  (let ((this-node (position-node position))
	(this-node-offset (position-node-offset position)))
    (assert (if (btree-node-leafp tree this-node)
		(null child-to-delete)
		child-to-delete))

    ;; 1. Delete the key/value pair from the node
    (btree-node-delete-from-node tree this-node this-node-offset
				 :child-to-delete child-to-delete)

    ;; 2. Rebalance the tree after deletion

    (multiple-value-bind (action
			  parent-keyval-position
			  destination-keyval-position
			  sibling-info
			  sibling-child-position
			  destination-child-position)
	(btree-compute-underflow-action tree position)
      (declare (type (member :merge :shift-one) action))
      (case action
	;; Case 1: we do not need to merge sibling nodes
    	(:shift-one
	   (assert (eql this-node (position-node destination-keyval-position)))
	   (assert (eql (position-node (position-parent position))
			(position-node parent-keyval-position)))
	   (let* ((sibling-keyval-position sibling-info))
	     ;; move parent key/value into this node at the given position
	     (btree-mov tree
			parent-keyval-position destination-keyval-position
			nil                    :insert nil)
	     ;; Copy the child from the sibling into the destination
	     (btree-mov-child tree
			      sibling-child-position destination-child-position
			      :delete                :insert)
	     ;; move the sibling key/value into the key/value of the parent
	     (btree-mov tree
			sibling-keyval-position parent-keyval-position
			:delete                 :replace-key-value nil)))
	;; Case 2: we need to merge this node with a sibling node
	(:merge
	   (let ((sibling-node sibling-info)
		 (surviving-node (position-node destination-keyval-position))
		 (parent-node (position-node (position-parent position))))
	     (assert (eql (position-node (position-parent position))
			  (position-node parent-keyval-position)))
	     (debug-format "Decided to merge ~A~%           into  ~A~%   Parent: ~A~%"  sibling-node surviving-node parent-node)
	     (debug-format "Key/value from ~A~%          ===> ~A~%"  parent-keyval-position destination-keyval-position)
	     ;; move parent key/value into destination node at the given position
	     (btree-mov tree
			parent-keyval-position destination-keyval-position
			nil                    :insert nil)

	     ;; append the key/values from right node to the left node
	     (debug-format "After parent-mov: ~A ~%"  surviving-node)
	     (debug-format "Going to merge ~A~%         into  ~A~%"  sibling-node surviving-node)
	     (btree-join-nodes tree surviving-node sibling-node)
	     (debug-format "After merge:      ~A ~%"  surviving-node)
	     ;; Now we delete the parent key/value and right child
	     (let ((parent-keycount (btree-node-keycount tree parent-node)))
	       (cond
		 ;; If we are deleting the last key-value from the root, then
		 ;; replace the root with the surviving node
		 ((and (btree-node-rootp tree parent-node)
		       (= 1 parent-keycount))
		  (btree-replace-root tree :node surviving-node))
		 ;; Uh-oh, the parent has the min number of elements.  Recursively delete
		 ((= (btree-min-keys tree) parent-keycount)
		  (btree-delete-from-minimally-filled-node tree
							   parent-keyval-position
							   :child-to-delete :right))
		 ;; If the parent node is sufficiently filled, just remove the
		 ;; key/value and right child and be done with it
		 (t
		  (btree-node-delete-from-node tree parent-node
					       (position-node-offset parent-keyval-position)
					       :child-to-delete :right))))))))))

						     
						     
(defmethod btree-delete-from-internal-node (tree position &key  &allow-other-keys)
  ;; move key/value pair from child (either the first key/value in the RIGHT child
  ;; or the last key-value in the LEFT child) into this node and then delete it from
  ;; the child.

  ;; Snatch a value from the left child and switch it out for our key-value
  (let* ((successor-position (btree-position-successor tree position :direction :right)))
    ;; replace
    (btree-mov tree successor-position position :delete :replace-key-value nil)))


;;; Mapping over a btree
(defun btree-extreme-position (tree position direction)
  "Given a position in a btree that designates a node, returns the left-most or right-most
position in that node and all of its subnodes."
  (declare (type (member :left :right) direction ))
  (if (not position)
      nil
      (let ((node (btree-node-child-at-offset tree (position-node position) (position-node-offset position))))
	(if (btree-node-leafp tree node)
	    (make-instance 'btree-position
			   :node node
			   :node-offset (if (eql :left direction) 0 (1- (btree-node-keycount tree node)))
			   :parent position)
	    (let* ((child-offset  (if (eql :left direction) 0 (btree-node-keycount tree node)))
		   (child-position (make-instance
				    'btree-position
				    :node (btree-node-child-at-offset tree node child-offset)
				    :node-offset child-offset
				    :parent position)))
	      (btree-extreme-position tree child-position direction))))))

(defmethod btree-position-successor (tree position &key (direction :right) &allow-other-keys)
  "Position is the position of a key/value in the tree.  If position is "
  (let ((leafp (btree-node-leafp tree (position-node position))))
    (cond 
      ;; if we are not in a leaf, return the extremum of the left or right child
	((not leafp)
	 (assert (< (position-node-offset position)
		    (btree-node-keycount tree (position-node position))))
	 (btree-extreme-position tree
				 (make-instance
				  'btree-position
				  :node (position-node position)
				  :node-offset (if (eql :right direction)
						   (1+ (position-node-offset position))
						   (position-node-offset position))
				   :parent (position-parent position))
				 (if (eql :right direction) :left :right)))
      ;; if we are in a leaf but there is no next element, recurse
      ((and leafp
	    ;;
	    (if (eql :right direction)
		(= (1+ (position-node-offset position))
		   (btree-node-keycount tree (position-node position)))
		(= 0 (position-node-offset position))))
       (btree-position-successor tree (position-parent position) :direction direction))
      ;; if we are in a leaf with an obvious successor
      (t
       (make-instance 'btree-position
		      :node (position-node position)
		      :node-offset (if (eql :right direction)
				       (1+ (position-node-offset position))
				       (1- (position-node-offset position)))
		      :parent (position-parent position))))))


