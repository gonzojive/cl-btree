(in-package :btree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Generic Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(declaim (optimize (debug 3)))
(declaim (optimize (speed 3) (safety 0)))

(defun btree-min-keys (btree)
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

;(declaim (inline btree-make-position))
(defun btree-make-position (btree &key node node-offset parent)
  (declare (ignore btree))
  (make-instance 'btree-position :node node :node-offset node-offset :parent parent))

(defun btree-search (tree key &rest rest &key &allow-other-keys)
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
  

(defun btree-node-fullp (tree node)
  (= (btree-max-keys tree)
     (btree-node-keycount tree node)))

(defun key-at-position-matches (tree position key)
  (when (< (position-node-offset position)
	   (btree-node-keycount tree (position-node position)))
    (multiple-value-bind (k v)
	(btree-node-entry-at-offset tree (position-node position) (position-node-offset position))
      (declare (ignore v))
      (btree-value-equalp tree k key))))

(defun btree-position-for-key (tree key &key testp &allow-other-keys)
  (flet ((position-of-key-in-node (node parent-position)
	   (btree-make-position tree
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

(defun btree-insert (tree key value &rest rest &key &allow-other-keys)
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

(defun btree-delete (tree key &rest rest &key &allow-other-keys)
  (multiple-value-bind (position k v)
      (apply 'btree-search* tree key rest)
    (declare (ignore k v))
    (if position
	(btree-delete-at-position tree position)
	(values nil nil nil))))

(defun btree-insert-at-position (tree position key value &key left-child right-child &allow-other-keys)
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
			      :key key :value value :key-offset (position-node-offset position)
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

(defun btree-delete-at-position (tree position &key &allow-other-keys)
  (let* ((node (position-node position))
	 (leafp (btree-node-leafp tree node))
	 (minimally-filledp (= (btree-node-keycount tree node) (btree-min-keys tree))))
    (cond
      ((and leafp (or (not minimally-filledp)
		      (btree-node-rootp tree node)))
	;; if it's a leaf and it has more than the minimum number of elements
       (btree-node-delete-from-sufficiently-filled-leaf tree node (position-node-offset position)))
      
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
	  (btree-make-position tree
			       :node parent
			       :node-offset (if sibling-leftp sibling-offset-in-parent offset-in-parent)
			       :parent (position-parent parent-position)))
	 (destination-keyval-position
	  (if (eql :shift-one action)
	      ;; destination of a shift action is always the underflowed node
	      (btree-make-position tree
				   :node underflowed-node
				   :node-offset (if sibling-leftp 0 (btree-node-keycount tree underflowed-node))
				   :parent parent-position)
	      ;; destination of a merge action is always the left node
	      (btree-make-position tree
				   :node left-node
				   :node-offset (btree-node-keycount tree left-node)
				   :parent parent-keyval-position)))
	 (sibling-info
	  (if (eql :shift-one action)
	      ;; sibling-info of a shift action is the position in the sibling node
	      ;; from which we move a value into the parent's key/value position
	      (btree-make-position tree
				   :node sibling-node
				   :node-offset (if (not sibling-leftp) 0 (1- (btree-node-keycount tree sibling-node)))
				   :parent (btree-make-position tree
								:node parent
								:node-offset sibling-offset-in-parent
								:parent (position-parent parent-position)))
	      ;; sibling-info of a merge action is the right node (the discarded node)
	      right-node))
	 (sibling-child-position
	  (when (eql :shift-one action)
	    ;; sibling-info of a shift action is the position in the sibling node
	    ;; from which we move a value into the parent's key/value position
	    (btree-make-position tree
				 :node sibling-node
				 :node-offset (if (not sibling-leftp) 0 (btree-node-keycount tree sibling-node))
				 :parent (position-parent sibling-info))))
	 (destination-child-position
	  (when (eql :shift-one action)
	      ;; sibling-info of a shift action is the position in the sibling node
	      ;; from which we move a value into the parent's key/value position
	    (btree-make-position tree
				 :node underflowed-node
				 :node-offset (if sibling-leftp 0 (1+ (btree-node-keycount tree underflowed-node)))
				 :parent parent-position))))
    (values action
	    parent-keyval-position
	    destination-keyval-position
	    sibling-info
	    sibling-child-position
	    destination-child-position)))
	 
(defun  btree-delete-from-minimally-filled-node (tree position &key child-to-delete &allow-other-keys)
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

						     
						     
(defun btree-delete-from-internal-node (tree position &key  &allow-other-keys)
  ;; move key/value pair from child (either the first key/value in the RIGHT child
  ;; or the last key-value in the LEFT child) into this node and then delete it from
  ;; the child.

  ;; Snatch a value from the left child and switch it out for our key-value
  (let* ((successor-position (btree-position-successor tree position :direction :right)))
    ;; replace
    (btree-mov tree successor-position position :delete :replace-key-value nil)))

;;; Mapping over a btree
(defun btree-map (tree map-fn &key start end value from-end &allow-other-keys)
    ;; FIXME TODO handle all the arguments
  (let* ((root (btree-root tree))
	 (root-keycount (btree-node-keycount tree root)))
    (unless (= 0 root-keycount)
      (let* ((direction (if from-end :left :right))
	     (pos (if (btree-node-leafp tree root)
		      (btree-make-position tree
					   :node root
					   :node-offset (if from-end (- root-keycount 1) 0)
					   :parent nil)
		      (btree-extreme-position tree
					      (btree-make-position tree
								   :node root
								   :node-offset (if from-end root-keycount 0)
								   :parent nil)
					      (if from-end :right :left)))))
	(loop :until (null pos)
	      :do (multiple-value-bind (key val)
		      (btree-node-entry-at-offset tree (position-node pos) (position-node-offset pos))
		    (funcall map-fn key val))
	      :do (setf pos (btree-position-successor tree pos :direction direction)))))))
	

    
(defun btree-extreme-position (tree position direction)
  "Given a position in a btree that designates a node, returns the left-most or right-most
position in that node and all of its subnodes."
  (declare (type (member :left :right) direction ))
  (if (not position)
      nil
      (let ((node (btree-node-child-at-offset tree (position-node position) (position-node-offset position))))
	(if (btree-node-leafp tree node)
	    (btree-make-position tree
				 :node node
				 :node-offset (if (eql :left direction) 0 (1- (btree-node-keycount tree node)))
				 :parent position)
	    (let* ((child-offset  (if (eql :left direction) 0 (btree-node-keycount tree node)))
		   (child-position (btree-make-position tree
							:node node
							:node-offset child-offset
							:parent position)))
	      (btree-extreme-position tree child-position direction))))))

(defun btree-position-successor (tree position &key (direction :right) &allow-other-keys)
  "Position is the position of a key/value in a btree.  If position is
at an extreme end of the tree it is legal to return NIL to indicate
that there is no successor.  Otherwise, the returned value must be a
valid position in the tree from which a KEY/VALUE pair may be
fetched."
  (let ((leafp (btree-node-leafp tree (position-node position))))
    (cond 
      ;; if we are not in a leaf, return the extremum of the left or right child
	((not leafp)
	 (assert (< (position-node-offset position)
		    (btree-node-keycount tree (position-node position))))
	 (btree-extreme-position tree
				 (btree-make-position tree
						      :node (position-node position)
						      :node-offset (if (eql :right direction)
								       (1+ (position-node-offset position))
								       (position-node-offset position))
						      :parent (position-parent position))
				 (if (eql :right direction) :left :right)))
      ;; if we are in a leaf but there is no next element and we are going RIGHT,
      ;; then return the parent node
      ((and leafp
	    (eql direction :right)
	    (= (1+ (position-node-offset position))
	       (btree-node-keycount tree (position-node position))))
       (labels ((first-valid-parent-position (position)
		  (when-let (parent-pos (position-parent position))
		    (let* ((offset-in-parent (position-node-offset parent-pos))
			   (offset-in-parent-valid? (< offset-in-parent
						       (btree-node-keycount tree (position-node parent-pos)))))
		      (if offset-in-parent-valid?
			  parent-pos
			  (first-valid-parent-position parent-pos))))))
	 (first-valid-parent-position position)))
      ((and leafp
	    (eql direction :left)
	    (= 0 (position-node-offset position)))

       (labels ((first-valid-parent-position-to-left (position)
		  (when-let (parent-pos (position-parent position))
		    (let* ((offset-in-parent (position-node-offset parent-pos))
			   (offset-in-parent-valid? (< 0 offset-in-parent)))
		      (if offset-in-parent-valid?
			  (btree-make-position tree
					       :node (position-node parent-pos)
					       :node-offset (- offset-in-parent 1)
					       :parent (position-parent parent-pos))
			  (first-valid-parent-position-to-left parent-pos))))))
	 (first-valid-parent-position-to-left position)))
      ;; if we are in a leaf with an obvious successor
      (t
       (btree-make-position tree
			    :node (position-node position)
			    :node-offset (if (eql :right direction)
					     (1+ (position-node-offset position))
					     (1- (position-node-offset position)))
			    :parent (position-parent position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Memory Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    :documentation "Array of key-value conses.  Capacity is MAX-KEYS+1"
    :type (vector cons))
   (children
    :initarg :children
    :accessor node-children
    :type (vector memory-btree-node)
    :documentation "Array of child nodes.  Capicity is MAX-KEYS+2.
Leafs have 0-length child array."))
  (:documentation "btree stored in memory"))

(defmethod print-object ((obj memory-btree-node) s)
  (print-unreadable-object (obj s :type nil :identity nil)
    (format s "NODE keyvals: ~S children: ~S"
	    (node-keyvals obj)
	    (node-children obj))))

(defun btree-make-node (btree &key &allow-other-keys)
  (make-instance 'memory-btree-node
		 :keyvals (make-array (1+ (btree-max-keys btree)) :fill-pointer 0 :adjustable nil)
		 :children (make-array (+ 2 (btree-max-keys btree)) :fill-pointer 0 :adjustable nil)))

(defmethod initialize-instance ((btree memory-btree)  &key &allow-other-keys)
  (call-next-method)
  (setf (btree-root btree) (btree-make-node btree))
  btree)

(defun btree-node-keycount (tree node)
  (or (when-let (keyvals (node-keyvals node)) (length keyvals))
      0))

(defun btree-value-equalp (tree value test-value)
  (let ((less (btree-test tree)))
    (and (not (funcall less value test-value))
	 (not (funcall less test-value value)))))

(defun btree-nodep (tree node)
  (and (typep tree 'memory-btree)
       (typep node 'memory-btree-node)))

(defun btree-node-leafp (tree node)
  (= 0 (length (node-children node))))

(defun btree-node-rootp (tree node)
  "EQL test is default."
  (eql (btree-root tree) node))

#+nil
(defun btree-node-fullp (tree node)
  (= (btree-max-keys tree)
     (length (node-keyvals node))))

(defun insert-into-array (array value position)
  (vector-push nil array)
  (replace array array :start2 position :start1 (1+ position))
  (setf (aref array position) value)
  array)

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
  (declare (type (vector cons) vec)
           (type fixnum start end)
	   (type function less-than key)
	   (optimize (speed 3) (safety 0)))
  (if (= 0 (- end start))
      start
      (let* ((center (the fixnum (floor (+ start end) 2)))
	     (value (aref vec center))
	     (key-value (funcall key value)))
	(if (= (- end start) 1)
	    (if (funcall less-than key-value find-value)
		(1+ start)
		start)
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

(defun btree-node-offset-for-key (tree
                                  node
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

(defun btree-node-child-at-offset  (tree node offset)
  (assert (not (btree-node-leafp tree node)))
  (assert (< offset (length (node-children node))))
  (assert (btree-nodep tree   (aref (node-children node) offset)))
  (aref (node-children node) offset))


(defun vector-append (v1 v2)
  (declare (type vector v1 v2))
  (let ((original-v1-length (length v1)))
    (incf (fill-pointer v1) (length v2))
    (replace v1 v2 :start1 original-v1-length :start2 0)))

(defun btree-join-nodes (tree node other-node)
  (assert (not (xor (btree-node-leafp tree node) (btree-node-leafp tree other-node))))
  ;; first insert all the key/values
  ;; second inset all the children
  (vector-append (node-children node) (node-children other-node))
  (vector-append (node-keyvals node) (node-keyvals other-node)))
          


(defun btree-node-entry-at-offset (tree node offset)
  (assert (< offset (btree-node-keycount tree node)))
  (let ((cons (aref (node-keyvals node) offset)))
    (values (car cons) (cdr cons))))


(defun btree-replace-root (tree
                           &key node key value left-child right-child)
  (debug-format "Replacing root: ~A ~A~%" left-child right-child)
  (if node
      (setf (btree-root tree) node)
      (let ((root (btree-make-node tree)))
	(setf (btree-root tree) root)
	(btree-node-insert-unfilled tree root 0 key value
				    :right-child right-child  :left-child left-child)
	root)))

(defun btree-node-insert-unfilled  (tree node offset key value
                                    &key left-child right-child &allow-other-keys)
  (assert (or (not (and left-child right-child))
	      (and (btree-node-rootp tree node) (= 0 (length (node-keyvals node))))))
  (insert-into-array (node-keyvals node) (cons key value) offset)
  (when left-child
    (insert-into-array (node-children node) left-child offset))
  (when right-child
    (insert-into-array (node-children node) right-child (1+ offset)))
  tree)

(defun btree-node-insert-child  (tree node offset child-node &key &allow-other-keys)
  (insert-into-array (node-children node) child-node offset))

(defun btree-node-delete-child  (tree node
				     offset &key &allow-other-keys)
  (with-accessors ((children node-children))
      node
    (setf children (delete-if (constantly t) children :start offset :count 1))))

(defun btree-node-replace-key-value  (tree node
					  offset key value &key &allow-other-keys)
  (setf (aref (node-keyvals node) offset) (cons key value)))

(defun btree-node-delete-from-sufficiently-filled-leaf (tree node
							    offset &key &allow-other-keys)
  (assert (> (btree-node-keycount tree node) (btree-min-keys tree)))
  (btree-node-delete-from-node tree node offset))

(defun btree-node-delete-from-node (tree node
					offset &key child-to-delete &allow-other-keys)
  (declare (type (member :left :right nil) child-to-delete))
  (with-accessors ((keyvals node-keyvals)
		   (children node-children))
      node
    (setf keyvals (delete-if (constantly t) keyvals :start offset :count 1))
    (when child-to-delete
      (assert (not (btree-node-leafp tree node)))
      (let ((child-offset (if (eql :left child-to-delete) offset (1+ offset))))
	(setf children (delete-if (constantly t) children :start child-offset :count 1))))))


(defun btree-node-split (tree node
			     &key key key-offset value left-child right-child)
  "When a given a node, splits it in half.  The left half of the keys/values
are kept in this node, and a new node is created with the right half.  Returns the following
values:
1.  Left node.
2.  Right node.
3.  Median key.
4.  Median value."
  (let ((key-offset (or key-offset (btree-node-offset-for-key tree node key)))
	(median-offset (floor (/ (btree-max-keys tree) 2))))

    (btree-node-insert-unfilled tree node key-offset key value
				   :left-child left-child :right-child right-child)

    (multiple-value-bind (median-key median-value)
	(btree-node-entry-at-offset tree node median-offset)

      (let ((right (let ((r (btree-make-node tree)))
		     ;(debug-format "Mapping ~A into right keyvals~%" (node-keyvals node))
		     (let ((num-keyvals (- (length (node-keyvals node)) (1+ median-offset))))
		       (setf (fill-pointer (node-keyvals r)) num-keyvals)
		       (replace (node-keyvals r) 
				(node-keyvals node)
				:start2 (1+ median-offset))
		     ;(debug-format "Mapped into ~A~%" (node-keyvals r))
		       (when (not (btree-node-leafp tree node))
			 ;; absorb the median's right child into the new right node
			 (setf (fill-pointer (node-children r)) (1+ num-keyvals))
			 (replace (node-children r) 
				  (node-children node)
				  :start2 (1+ median-offset))))
		     r))
	    (left (progn
		    (setf (fill-pointer (node-keyvals node)) median-offset)
		    (when (not (btree-node-leafp tree node))
		      ;; absorb the median's left child into the new left node
		      (setf (fill-pointer (node-children node)) (1+ median-offset)))
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


