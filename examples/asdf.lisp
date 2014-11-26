;;;; asdf.lisp --- Example: print ASDF systems as trees.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:print-tree.examples.asdf
  (:use
   #:cl)

  (:import-from #:utilities.print-tree
   #:*use-unicode?*

   #:print-tree
   #:make-node-printer))

(cl:in-package #:print-tree.examples.asdf)

(defmethod print-component ((target stream) (depth t) (component asdf:component))
  (princ (asdf:component-name component) target)
  ;; Return true to indicate that details can be printed for
  ;; COMPONENT.
  t)

(defmethod print-details ((target stream) (depth t) (component asdf:component))
  (format target "Pathname   ~A~@:_~
                  Encoding   ~A~@:_~
                  Depends-on ~A"
          (asdf:component-pathname   component)
          (asdf:component-encoding   component)
          (asdf:component-depends-on 'asdf:load-op component)))

(defmethod asdf:component-children ((component t))
  '())

(labels ((do-it (details?)
           (fresh-line)
           (terpri)
           (print-tree *standard-output* (asdf:find-system :utilities.print-tree)
                       (make-node-printer #'print-component
                                          (when details? #'print-details)
                                          #'asdf:component-children))))

  (do-it nil)
  (do-it t)

  (let ((*use-unicode?* nil))
    (do-it nil)
    (do-it t)))
