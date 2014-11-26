;;;; package.lisp --- Package definition for the utilities.print-tree system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:utilities.print-tree
  (:use
   #:cl
   #:alexandria)

  (:export
   #:*use-unicode?*)

  (:export
   #:print-tree
   #:print-subtree
   #:print-node-contents)

  (:export
   #:make-node-printer
   #:make-folding-node-printer)

  (:documentation
   "This package provides functions for printing tree structures.

    The main entry point is the `print-tree' function. It accepts a
    stream, a tree object and a printer function.

    In common cases, tree printer functions can be creating using
    `make-node-printer' or `make-folding-node-printer'."))
