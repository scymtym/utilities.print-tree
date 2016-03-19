;;;; examples.lisp --- Test examples.
;;;;
;;;; Copyright (C) 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.print-tree.test)

(def-suite :utilities.print-tree.examples
  :in :utilities.print-tree
  :description
  "Test suite that makes sure the examples included in the
   utilities.print-tree system do not break.")

(in-suite :utilities.print-tree.examples)

(defun test-example (file)
  (is (not (emptyp (with-output-to-string (*standard-output*)
                     (load file))))))

(macrolet
    ((define-examples ()
       (let ((examples (asdf:component-children
                        (asdf:find-component :utilities.print-tree '("examples")))))
         `(progn
            ,@(mapcar
               (lambda (example)
                 (let* ((name      (asdf:component-name example))
                        (test-name (symbolicate
                                    'example. (read-from-string name))))
                   `(test ,test-name
                      ,(format nil "Load the ~S example." name)
                      (test-example ,(asdf:component-pathname example)))))
               examples)))))
  (define-examples))
