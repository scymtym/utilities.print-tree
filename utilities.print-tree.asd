;;;; utilities.print-tree.asd --- System definition for utilities.print-tree.
;;;;
;;;; Copyright (C) 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :utilities.print-tree
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "This system provides simple facilities for printing tree structures."
  :depends-on  (:alexandria)
  :encoding    :utf-8
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "print-tree")))

                (:static-file "README.org")

                (:module     "examples"
                 :components ((:static-file "minimal.lisp")
                              (:static-file "asdf.lisp"))))

  :in-order-to ((test-op (test-op :utilities.print-tree/test))))

(defsystem :utilities.print-tree/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for the utilities.print-tree system."
  :defsystem-depends-on (:uiop)
  :depends-on  (:alexandria
                (:version :fiveam               "1.3")
                (:version :utilities.print-tree (:read-file-form "version-string.sexp")))
  :encoding    :utf-8
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "print-tree")
                              (:file       "examples")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :utilities.print-tree/test))))
  (uiop:symbol-call '#:utilities.print-tree.test '#:run-tests))
