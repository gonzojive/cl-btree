;;;; jwacs.asd
;;;
;;; This is the system definition file for the jwacs project.
;;; It defines the asdf system plus any extra asdf operations
;;; (eg test-op).

(defpackage :btree-system
  (:use :cl :asdf)
  (:export
   #:*version*
   #:*executable-name*))

(in-package :btree-system)

;;;; ======= System definition =====================================================================
(asdf:defsystem btree
  :version *version*
  :author "Red Daly"
  :licence "MIT License <http://www.opensource.org/licenses/mit-license.php>"
  :serial t
  :components ((:module
		"src"
		:components
		((:file "package")
		 #+nil(:file "generics" :depends-on ("package"))
		 #+nil(:file "memory" :depends-on ("generics"))
		 )))

  :depends-on (:alexandria))

(defsystem btree-tests
  :components ((:module "test"
                        :components ((:file "test-package")
				     (:file "memory-tests" :depends-on ("test-package"))
				     )))
  :depends-on ("btree" "hu.dwim.stefil"))
