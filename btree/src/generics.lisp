(in-package :btree)

;; public generics
(defgeneric btree-insert (tree key value &key &allow-other-keys)
  (:documentation "Inserts the given key and value into the btree."))

(defgeneric btree-search (tree key &key &allow-other-keys)
  (:documentation "Returns 3 values: the value and the ky of the entry found, and T if one
was actually found (in case nil nil are the key and value)."))

(defgeneric btree-map (tree map-fn &key start end value from-end &allow-other-keys)
  (:documentation "Returns 3 values: the key and value of of the entry found, and T if one
was actually found (in case nil nil are the key and value)."))

(defgeneric btree-max-keys (btree)
  (:documentation "The maximum number of keys allowed in each node."))

(defgeneric btree-min-keys (btree)
  (:documentation "The minimum number of keys allowed in each node."))


;; private generics
(defgeneric btree-root (tree)
  (:documentation "Returns the root node of a btree"))

(defgeneric btree-nodep (tree node)
  (:documentation "Returns T if the given thing is a node in the given tree.  If T
then that node must conform to this protocol."))

(defgeneric btree-node-leafp (tree node)
  (:documentation "Returns T if the given node is a leaf node."))

(defgeneric btree-node-rootp (tree node)
  (:documentation "Returns T if the given node is a leaf node."))

(defgeneric btree-node-fullp (tree node)
  (:documentation "Returns T if the node is full."))
 

(defgeneric btree-node-insert (tree node key &key value left-child right-child &allow-other-keys)
  (:documentation "Inserts the given KEY into the given NODE for a given TREE.

This works for both an internal, leaf, or root nodes, but with more
constraints than on the function btree-insert.

If the NDOE is full: split the node in half, with at most n/2 nodes on
one side and n/2 nodes on the other side of some median value.  All
the values to the left of the median are kept in NODE, while those on
the right are added to a new node.  The median is then added to the
parent node with :left-child value of NODE and a :right-child value of
the new node."))

(defgeneric btree-replace-root (tree &key key value left-child right-child)
  (:documentation "Called in the process of insertion if it is necessary to split the
root."))

(defgeneric btree-node-insert-at-position (tree node position key &key value left-child right-child &allow-other-keys)
  (:documentation "Inserts the given KEY and VALUE at the given POSITION in NODE.  This
is only to be called on a node that is not full, and a node that is a LEAF."))

(defgeneric btree-node-split (tree node &key key value left-child right-child)
  (:documentation "When a given a node, splits it in half.  The left half of the keys/values
are kept in this node, and a new node is created with the right half.  Returns the following
values:
1.  Left node.
2.  Right node.
3.  Median key.
4.  Median value."))

(defgeneric btree-node-entry-at-position (tree node position)
  (:documentation "Returns 2 values: the key and the value at the given position in
the given tree and node, as returned by BTREE-POSITION-FOR-KEY"))

(defgeneric btree-position-for-key (tree key &key maxp &allow-other-keys)
  (:documentation "Given a btree and a key, returns 2 values: 
1. The node into which the key either is found or could be added if it does not exist.
2. A valid position that indicates where in the given node that key may be found
or could be inserted.

Even if an node is full both values should be non-null.  Duplicate entries are allowed
and if MAXP is non-null, then the maximum position at which the key may be INSERTED is
returned (so 1 past the last value that matches key)."))

(defgeneric btree-node-position-for-key (tree node key &key maxp &allow-other-keys)
  (:documentation "Given a btree and a key, returns 2 values: 
1. A position that indicates where in the given node that key may be found or could be
inserted."))

(defgeneric btree-node-child-at-position (tree node position)
  (:documentation "If leftp is T, returns the child node to the left of the POSITIONth
element stored in this node.  If leftp is NIL, returns the right child."))

(defgeneric btree-value-equalp (tree value test-value)
  (:documentation "Returns non-nil iff value equals test-value
according to the comparison used by the btree."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Generic Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod btree-min-keys (btree)
  (floor (btree-max-keys btree) 2))

(defmethod btree-search (tree key &rest rest &key &allow-other-keys)
  "Search is performed in the typical manner, analogous to that in a
binary search tree. Starting at the root, the tree is traversed top to
bottom, choosing the child pointer whose separation values are on
either side of the value that is being searched."
  (multiple-value-bind (node position)
      (apply #'btree-position-for-key tree key rest)
    (when (or node position)
      (multiple-value-bind (k v)
	  (btree-node-entry-at-position tree node position)
	(if (btree-value-equalp tree k key)
	    (values v k t)
	    (values nil nil nil))))))