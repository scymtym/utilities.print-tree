;;;; print-tree.lisp --- Unit tests for the print-tree function.
;;;;
;;;; Copyright (C) 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.print-tree.test)

(in-suite :utilities.print-tree)

(test print-tree.smoke
  "Smoke test for the `print-tree' function."

  (mapc (lambda (spec)
          (destructuring-bind (tree expected) spec
            (let ((result (with-output-to-string (stream)
                            (print-tree
                             stream tree
                             (make-node-printer
                              (lambda (stream depth node)
                                (format stream "~A@~D" (car node) depth))
                              nil
                              #'cdr)))))
              (is (string= result expected)))))
        `(((1) "1@0")

          ((1 (2) (3 (4) (5)))
           ,(format nil "1@0~@
                         ├─2@1~@
                         └─3@1~@
                         ~2@T├─4@2~@
                         ~2@T└─5@2")))))
