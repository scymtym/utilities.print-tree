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

                (:static-file "README.org")))
