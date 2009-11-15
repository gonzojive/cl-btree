(in-package :btree-tests)

(declaim (optimize (debug 3)))

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

(deftest test-emulates-hashmap (hash &rest btree-initargs)
  (let ((b (apply #'make-instance 'btree::memory-btree btree-initargs)))
    (maphash #'(lambda (k v) (btree-insert b k v)) hash)
    (maphash #'(lambda (k v)
		 (is (equal v (btree-search b k))))
	     hash)
    b))

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
  (ordered-in-ordered-out (ordered-listn 3000) :max-keys 15))

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

(deftest example-insert-and-search ()
  (let ((b (plist->btree '(1 "One" 2 "Two" 3 "Three"))))
    (is (equal "One" (btree-search b 1)))
    (is (equal "Two" (btree-search b 2)))
    (is (equal "Three" (btree-search b 3)))
    (is (equal nil (btree-search b 2.5)))))

(deftest easy-delete ()
  (let ((b (plist->btree '(1 "One" 2 "Two" 3 "Three") :max-keys 3)))
    (is (equal "One" (btree-search b 1)))
    (is (equal "Two" (btree-search b 2)))
    (is (equal "Three" (btree-search b 3)))
    (is (equal nil (btree-search b 2.5)))
    (btree-delete b 1)
    (btree-delete b 3)
    (is (null (btree-search b 1)))
    (is (equal "Two" (btree-search b 2)))
    (is (null (btree-search b 3)))))

(deftest internal-delete ()
  (let ((b (plist->btree '(1 "One" 2 "Two" 3 "Three"
			   0 "Zero" 1/2 "One Half")
			 :max-keys 2)))
    (is (equal "One" (btree-search b 1)))
    (is (equal "Two" (btree-search b 2)))
    (is (equal "Three" (btree-search b 3)))
    (is (equal "Zero" (btree-search b 0)))
    (is (equal "One Half" (btree-search b 1/2)))

    (btree-delete b 1/2) ;1/2 is the only value in an internal node

    (is (null (btree-search b 1/2)))

    (is (equal "One" (btree-search b 1)))
    (is (equal "Two" (btree-search b 2)))
    (is (equal "Three" (btree-search b 3)))
    (is (equal "Zero" (btree-search b 0)))

    b))

(deftest minimal-leaf-delete ()
  (let ((b (plist->btree '(1 "One" 2 "Two" 3 "Three")
			 :max-keys 2)))
    (is (equal "One" (btree-search b 1)))
    (is (equal "Two" (btree-search b 2)))
    (is (equal "Three" (btree-search b 3)))

    (btree-delete b 1) ;delete the only key in the left child

    (is (equal "Two" (btree-search b 2)))
    (is (equal "Three" (btree-search b 3)))
    (is (null (btree-search b 1)))

    b))

(deftest minimal-delete-right-leaf ()
  (let ((b (plist->btree '(1 "One" 2 "Two" 3 "Three") :max-keys 2)))
    (is (equal "One" (btree-search b 1)))
    (is (equal "Two" (btree-search b 2)))
    (is (equal "Three" (btree-search b 3)))

    (btree-delete b 3) ;2 is in the root, which is an internal node only value in an internal node

    (is (equal "One" (btree-search b 1)))
    (is (null (btree-search b 3)))
    (is (equal "Two" (btree-search b 2)))
    b))


(deftest minimal-delete-left-leaf ()
  (let ((b (plist->btree '(1 "One" 2 "Two" 3 "Three") :max-keys 2)))
    (is (equal "One" (btree-search b 1)))
    (is (equal "Two" (btree-search b 2)))
    (is (equal "Three" (btree-search b 3)))

    (btree-delete b 1) ;2 is in the root, which is an internal node only value in an internal node

    (is (null (btree-search b 1)))

    (is (equal "Two" (btree-search b 2)))
    (is (equal "Three" (btree-search b 3)))
    b))

(deftest minimal-delete-everything ()
  "Creates a hash table with 200 elements, adds them all to the btree, then
removes them one by one and checks that the hash table and btree still look alike."
  (let* ((plist (loop :for i :from -100 :upto 100
		      :collect i
		      :collect (format nil "=>~A" i)))
	 (b (plist->btree plist :max-keys 2)))
    (flet ((delete-and-check (hash)
	     (let ((i (block xxx (maphash #'(lambda (k v) (return-from xxx k)) hash))))
	       (btree-delete b i)
	       (remhash i hash)
	       (is (null (btree-search b i)))
	       (maphash #'(lambda (k v)
			    (is (equal v (btree-search b k))))
			hash))))
      (let ((hash  (plist-hash-table plist)))
	(dotimes (i 200)
	  (delete-and-check hash))))))

(deftest internal-delete-root ()
  (let ((b (plist->btree '(1 "One" 2 "Two" 3 "Three" 0 "Zero" 4 "Four") :max-keys 2)))
    (is (equal "One" (btree-search b 1)))
    (is (equal "Two" (btree-search b 2)))
    (is (equal "Three" (btree-search b 3)))
    (is (equal "Zero" (btree-search b 0)))
    (is (equal "Four" (btree-search b 4)))

    (btree-delete b 2) ;2 is in the root, which is an internal node only value in an internal node

    (is (null (btree-search b 2)))

    (is (equal "One" (btree-search b 1)))
    (is (equal "Three" (btree-search b 3)))
    (is (equal "Zero" (btree-search b 0)))
    (is (equal "Four" (btree-search b 4)))

    b))



(deftest hashmap-like1 ()
  (test-emulates-hashmap (plist-hash-table '(1 "One" 2 "Two" 3 "Three")))
  (test-emulates-hashmap
   (plist-hash-table (loop :for i :from 0 :upto 1000
			   :collect (random 1000000)
			   :collect (format nil "~A ~A" (random 100) (random 5000))))))

