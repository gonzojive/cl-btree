(defpackage btree-tests
  (:use :common-lisp :btree :hu.dwim.stefil :alexandria)
  (:export #:run-tests))

(in-package btree-tests)

(defsuite btree-tests)

(in-suite btree-tests)

(defun run-tests ()
  (btree-tests))