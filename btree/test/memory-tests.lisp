(in-package :btree-tests)

(declaim (optimize debug))

(in-suite btree-tests)

(deftest testing-environment-works ()
  (is (= 5 5))
  (is (null nil))
  t)

(deftest new-tree-no-error ()
  (is t))

(deftest insert-not-fullp-leaf ()
  (let ((b (make-instance 'btree::memory-btree :max-keys 5)))
    (dotimes (i 5)
      (btree:btree-insert b i (format nil "Value ~A" i)))
    (let ((r (btree::btree-root b)))
      (is r)
      (is (btree::btree-node-leafp b r)))))

(deftest insert-fullp-leaf ()
  (let ((b (make-instance 'btree::memory-btree :max-keys 5)))
    (dotimes (i 5)
      (btree:btree-insert b i (format nil "Value ~A" i)))
    (is (btree::btree-node-leafp b (btree::btree-root b)))
    (btree:btree-insert b 5 (format nil "Value ~A" 5))
    ;; add a value to the tree and ensure the root is no longer a leaf
    (is (not (btree::btree-node-leafp b (btree::btree-root b))))
    b))


(deftest root-ordering ()
  (let ((b (make-instance 'btree::memory-btree :max-keys 5)))
    (dotimes (i 5)
      (btree:btree-insert b i (format nil "Value ~A" i)))
    (let ((root-keys (map 'list #'car (btree::node-keyvals (btree::btree-root b)))))
      (is (= 0 (elt root-keys 0)))
      (is (= 1 (elt root-keys 1)))
      (is (= 2 (elt root-keys 2)))
      (is (= 3 (elt root-keys 3)))
      (is (= 4 (elt root-keys 4))))))

(deftest insert-hundreds-of-keyvals ()
  (let ((b (make-instance 'btree::memory-btree :max-keys 3)))
    (dotimes (i 500)
      (let ((num i))
	(btree:btree-insert b num (format nil "Value ~A" num))))

    (is (not (btree::btree-node-leafp b (btree::btree-root b))))
    b))

(defun tree-sizen (n)
  (let ((b (make-instance 'btree::memory-btree :max-keys 3)))
    (dotimes (i n)
      (btree:btree-insert b i (format nil "Value ~A" i)))
    b))