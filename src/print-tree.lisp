;;;; print-tree.lisp --- Implementation of the utilities.print-tree system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.print-tree)

;;; Constants

;; For Unicode

(define-constant +tree-child/inner+ "├─" :test #'string=)

(define-constant +tree-child/final+ "└─" :test #'string=)

(defun tree-child/unicode (kind)
  (ecase kind
    (:inner +tree-child/inner+)
    (:final +tree-child/final+)))

(define-constant +tree-prefix/final+ "  " :test #'string=)

(define-constant +tree-prefix/inner+ "│ " :test #'string=)

(defun tree-prefix/unicode (kind)
  (ecase kind
    (:root  "")
    (:inner +tree-prefix/inner+)
    (:final +tree-prefix/final+)))

;; For ASCII

(define-constant +tree-child/inner/ascii+ "+-" :test #'string=)

(define-constant +tree-child/final/ascii+ "`-" :test #'string=)

(defun tree-child/ascii (kind)
  (ecase kind
    (:inner +tree-child/inner/ascii+)
    (:final +tree-child/final/ascii+)))

(define-constant +tree-prefix/inner/ascii+ "| " :test #'string=)

(define-constant +tree-prefix/final/ascii+ "  " :test #'string=)

(defun tree-prefix/ascii (kind)
  (ecase kind
    (:root  "")
    (:inner +tree-prefix/inner/ascii+)
    (:final +tree-prefix/final/ascii+)))

;;; Variables

(defvar *use-unicode?* t)

(defun tree-child (kind)
  (if *use-unicode?*
      (tree-child/unicode kind)
      (tree-child/ascii kind)))

(defun tree-prefix (kind)
  (if *use-unicode?*
      (tree-prefix/unicode kind)
      (tree-prefix/ascii kind)))

;;; Printing functions

(declaim (ftype (function (stream t function)) print-tree))
(defun print-tree (stream root printer)
  "Print the tree ROOT to STREAM using PRINTER.

   PRINTER is called with four arguments, STREAM, ROOT, the
   keyword :root and the integer 0, indicating depth zero. PRINTER
   will usually call `print-node', `print-subree' and
   `print-node-contents' to do the actual work.

   Functions suitable for use as PRINTER can, for many common cases,
   be constructed via `make-node-printer' and
   `make-folding-node-printer'."
  (pprint-logical-block (stream (list root))
    (funcall printer stream root :root 0)))

(declaim (ftype (function (stream sequence function non-negative-integer))
                print-nodes))
(defun print-nodes (stream nodes printer depth)
  (let ((length (length nodes)))
    (loop :for element :in   nodes
          :for i       :from 1     :do
      (let* ((final?       (= i length))
             (child-string (tree-child (if final? :final :inner))))
        (write-string child-string stream)
        (funcall printer stream element final? depth)
        (unless final? (pprint-newline :mandatory stream))))))

(declaim (ftype (function (stream sequence function
                           &optional t non-negative-integer))
                print-subtree))
(defun print-subtree (stream nodes printer &optional final? (depth 0))
  (print-node-contents
   stream (lambda (stream depth)
            (print-nodes stream nodes printer depth))
   final? (1+ depth)))

(declaim (ftype (function (stream function &optional t non-negative-integer))
                print-node-contents))
(defun print-node-contents (stream printer &optional final? (depth 0))
  (pprint-logical-block (stream (list printer)
                                :per-line-prefix (tree-prefix (case final?
                                                                (:root :root)
                                                                ((nil) :inner)
                                                                ((t)   :final))))
    (funcall printer stream depth)))

;;; Node printer generators

(defun make-node-printer (first-line-printer rest-printer children)
  "Return a node printer function that used FIRST-LINE-PRINTER,
   REST-PRINTER and CHILDREN.

   FIRST-LINE-PRINTER is called with the destination stream, the depth
   of the node and the node and should print a one-line representation
   of the node to the stream. The function should return true if
   REST-PRINTER should be called to print more details for the node.

   REST-PRINTER is also called with the destination stream, the depth
   of the node and node. It is not restricted to only producing a
   single line, however.

   CHILDREN is called with a node and must return a (possibly empty)
   list of children of the node. "
  (labels ((printer (stream element final? depth)
             ;; Print first line of node using FIRST-LINE-PRINTER.
             ;; The call returns a Boolean to indicates whether more
             ;; lines should be printed using REST-PRINTER.
             (let ((rest?    (funcall first-line-printer stream depth element))
                   (children (funcall children element)))
               ;; When there is a REST-PRINTER and FIRST-LINE-PRINTER
               ;; indicated that more lines should be printed, start a
               ;; new line and print within a logical block.
               (when (and rest-printer rest?)
                 (pprint-newline :mandatory stream)
                 (flet ((content-printer (stream depth)
                          (print-node-contents
                           stream (rcurry rest-printer element)
                           (if children nil t) depth)))
                   (declare (dynamic-extent #'content-printer))
                   (print-node-contents
                    stream #'content-printer final? depth)))
               (when children
                 (pprint-newline :mandatory stream)
                 (print-subtree stream children #'printer final? depth)))))
    #'printer))

(defun make-folding-node-printer (first-line-printer rest-printer children
                                  predicate)
  "Like `make-node-printer', return a node printer function that used
   FIRST-LINE-PRINTER, REST-PRINTER and CHILDREN.

   PREDICATE is called to determine whether and to what extend nodes
   should be printed. When called with the depth of a node and node as
   its arguments, PREDICATE must return a list of \"bits\" of the node
   that should be printed. The following bits are recognized:

   :first

     Print first line of the node by calling FIRST-LINE-PRINTER?

   :content

     Print more details for the node by calling REST-PRINTER?

   :children

     Print children of the node?

   :children-ellipsis

     Print ellipsis of the form \"…\", if the node has children
     butlast the :children bit has not been specified?

   When PREDICATE returns T, all bits are printed."
  (labels ((printer (stream element final? depth)
             (labels ((print-first-line ()
                        (funcall first-line-printer stream depth element))
                      (print-ellipsis/first-line ()
                        (format stream "…"))

                      (print-content (children? &optional (printer rest-printer))
                        (when printer
                          (pprint-newline :mandatory stream)
                          (flet ((content-printer (stream depth)
                                   (print-node-contents
                                    stream (rcurry printer element)
                                    (if children? nil t) depth)))
                            (declare (dynamic-extent #'content-printer))
                            (print-node-contents
                             stream #'content-printer final? depth))))
                      (print-ellipsis/content (stream depth element)
                        (declare (ignore depth element))
                        (format stream "…"))

                      (print-children (children &optional (printer #'printer))
                        (when children
                          (pprint-newline :mandatory stream)
                          (print-subtree stream children printer final? depth)))
                      (print-ellipsis/children (stream element final? depth)
                        (declare (ignore element final? depth))
                        (format stream "…"))

                      (bit? (bit bits)
                        (or (eq bits t) (member bit bits :test #'eq))))
               (declare (dynamic-extent #'print-first-line #'print-ellipsis/first-line
                                        #'print-content #'print-ellipsis/content
                                        #'print-children #'print-ellipsis/children))
               (let* ((bits (funcall predicate depth element))
                      (first?             (bit? :first bits))
                      (rest?)
                      (content?           (bit? :content bits))
                      (children?          (bit? :children bits))
                      (children-ellipsis? (bit? :children-ellipsis bits))
                      (children           (funcall children element)))
                 ;; Print first line, if requested. The printer
                 ;; returns a Boolean (stored in REST?) to indicate
                 ;; whether the node has :content.
                 (if first?
                     (setf rest? (print-first-line))
                     (print-ellipsis/first-line))
                 ;; Print node content if requested and REST?
                 ;; indicates that the node actually has content.
                 (cond
                   ((and rest? content?)
                    (print-content children))
                   ((and rest? first?)
                    (print-content #'print-ellipsis/content)))
                 ;; Print children if requested.
                 (cond
                   (children?
                    (print-children children))
                   ((and children children-ellipsis?)
                    (print-children '(nil) #'print-ellipsis/children)))))))
    #'printer))
