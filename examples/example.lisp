;;;; example.lisp --- Very simple tree printing example.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:print-tree.examples.trivial
  (:use
   #:cl)

  (:import-from #:utilities.print-tree
                #:print-tree
                #:make-node-printer))

(cl:in-package #:print-tree.examples.trivial)

(let ((stream *standard-output*)
      (tree   '(0 (1 (2) (3)) (4) (5 (6)) (7 (8 (9) (10))) (11))))
  (pprint-logical-block (stream tree)
    (print-tree stream tree (make-node-printer
                             (lambda (stream depth node)
                               (declare (ignore depth))
                               (princ (first node) stream))
                             nil #'rest)))
  (terpri stream))
