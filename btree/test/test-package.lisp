(defpackage btree-tests
  (:use :common-lisp :btree :stefil)
  (:export #:run-tests))

(in-package btree-tests)

(defsuite btree-tests)

(in-suite btree-tests)

(defun run-tests ()
  (btree-tests))