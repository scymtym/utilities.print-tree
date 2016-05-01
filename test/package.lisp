;;;; package.lisp --- Package definition for the utilities.print-tree system.
;;;;
;;;; Copyright (C) 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:utilities.print-tree.test
  (:use
   #:cl
   #:alexandria

   #:utilities.print-tree

   #:fiveam)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the utilities.print-tree
    system."))

(cl:in-package #:utilities.print-tree.test)

(def-suite :utilities.print-tree
  :description
  "Test suite for the utilities.print-tree system.")

(defun run-tests ()
  (5am:run! :utilities.print-tree))
