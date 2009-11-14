(defpackage btree-tests
  (:use :common-lisp :btree :stefil :alexandria)
  (:export #:run-tests))

(in-package btree-tests)

(defsuite btree-tests)

(in-suite btree-tests)

(defun run-tests ()
  (btree-tests))