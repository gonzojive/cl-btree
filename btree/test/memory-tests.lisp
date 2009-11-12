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

(deftest ordered-in-ordered-out (ordered-list &key (max-keys 3))
  (let ((b (make-instance 'btree::memory-btree :max-keys max-keys))
	out)
    (dolist (i ordered-list)
      (btree:btree-insert b i i))
    (btree:btree-map b #'(lambda (k v) (push k out)))
    (is (equalp ordered-list (nreverse out)))))

(deftest sorts-like-sort (list &key (max-keys 3) (predicate #'<))
  (let ((b (make-instance 'btree::memory-btree :max-keys max-keys :predicate predicate))
	out)
    (dolist (i list)
      (btree:btree-insert b i i))
    (btree:btree-map b #'(lambda (k v) (push k out)))
    (is (equalp (sort (copy-list list) predicate)
		(nreverse out)))))

(defun ordered-listn (n)
  (loop :for i :from 0 :upto n :collect i))

(deftest many-ordered-when-mapped ()
  (ordered-in-ordered-out (ordered-listn 3))
  (ordered-in-ordered-out (ordered-listn 12))
  (ordered-in-ordered-out (ordered-listn 30))
  (ordered-in-ordered-out (ordered-listn 3000) :max-keys 15)
  (ordered-in-ordered-out (ordered-listn 3000) :max-keys 15)))

(defun plist->btree (plist &rest btree-args)
  (let ((b (apply #'make-instance 'btree::memory-btree btree-args)))
    (dolist (keyval (alexandria:plist-alist plist))
      (btree:btree-insert b (car keyval) (cdr keyval)))
    b))

(deftest mapping-values-hits ()
  (let ((b (plist->btree '(1 1 2 4))))
    (is 1 (btree-search b 1))
    (is 4 (btree-search b 2))))

(deftest mapping-values-hits2 ()
  (let ((b (plist->btree (loop :for i :from  0 :upto 300 :collect i :collect (+ i 10)))))
    (is 110 (btree-search b 100))
    (is 12 (btree-search b 2))))

(deftest mapping-values-misses ()
  (let ((b (plist->btree (loop :for i :from  0 :upto 300 :collect i :collect (+ i 10)))))
    (with-expected-failures
      (is (null (btree-search b -100)))
      (is (null (btree-search b 100.3))))))

(deftest sorting ()
  (sorts-like-sort '(3 4 13 6 2 3 14 6))
  (sorts-like-sort (mapcar #'(lambda (x) (format nil "blah blah ~A blah" x)) '(3 4 13 6 2 3 14 6))
		   :predicate #'string-lessp))
