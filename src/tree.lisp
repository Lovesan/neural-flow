;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2011, Dmitry Ignatiev <lovesan.ru@gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE

(in-package #:neural-flow)

;; Stolen from `trivial-garbage'

#+openmcl
(defvar *weak-pointers* (cl:make-hash-table :test 'eq :weak :value))

#+(or allegro openmcl lispworks)
(defstruct (weak-pointer (:constructor %make-weak-pointer))
  #-openmcl pointer)

(declaim (inline make-weak-pointer))
(defun make-weak-pointer (object)
  #+sbcl (sb-ext:make-weak-pointer object)
  #+(or cmu scl) (ext:make-weak-pointer object)
  #+clisp (ext:make-weak-pointer object)
  #+ecl (ext:make-weak-pointer object)
  #+allegro
  (let ((wv (excl:weak-vector 1)))
    (setf (svref wv 0) object)
    (%make-weak-pointer :pointer wv))
  #+openmcl
  (let ((wp (%make-weak-pointer)))
    (setf (gethash wp *weak-pointers*) object)
    wp)
  #+corman (ccl:make-weak-pointer object)
  #+lispworks
  (let ((array (make-array 1)))
    (hcl:set-array-weak array t)
    (setf (svref array 0) object)
    (%make-weak-pointer :pointer array)))

(declaim (inline weak-pointer-value))
(defun weak-pointer-value (weak-pointer)
  "If WEAK-POINTER is valid, returns its value. Otherwise, returns NIL."
  #+sbcl (values (sb-ext:weak-pointer-value weak-pointer))
  #+(or cmu scl) (values (ext:weak-pointer-value weak-pointer))
  #+clisp (values (ext:weak-pointer-value weak-pointer))
  #+ecl (values (ext:weak-pointer-value weak-pointer))
  #+allegro (svref (weak-pointer-pointer weak-pointer) 0)
  #+openmcl (values (gethash weak-pointer *weak-pointers*))
  #+corman (ccl:weak-pointer-obj weak-pointer)
  #+lispworks (svref (weak-pointer-pointer weak-pointer) 0))

;;Red-black tree

(declaim (inline %node %nleft %nright %nparent %nred %ndata %ncode
                 (setf %nleft) (setf %nright) (setf %nparent)
                 (setf %nred) (setf %ndata) (setf %ncode)))
(defstruct (%node (:constructor %node (data code parent red))
                  (:conc-name %n))
  (left nil :type (or null %node))
  (right nil :type (or null %node))
  (parent nil :type (or null %node))
  (red nil)
  data
  (code 0 :type (integer 0 #.most-positive-fixnum)))

(declaim (inline %tree %tree-root (setf %tree-root)))
(defstruct (%tree (:constructor %tree ())
                  (:copier %copy-tree))
  (root nil :type (or null %node)))

(declaim (inline rotate-left))
(defun %rotate-left (tree node)
  (declare (type %tree tree) (type %node node)
           (optimize (speed 3) (safety 0)))
  (let ((right (%nright node)))
    (when (setf (%nright node) (%nleft right))
      (setf (%nparent (%nleft right)) node))
    (if (setf (%nparent right) (%nparent node))
      (if (eq node (%nleft (%nparent node)))
        (setf (%nleft (%nparent node)) right)
        (setf (%nright (%nparent node)) right))
      (setf (%tree-root tree) right))
    (setf (%nleft right) node
          (%nparent node) right))
  (values))

(declaim (inline rotate-right))
(defun %rotate-right (tree node)
  (declare (type %tree tree) (type %node node)
           (optimize (speed 3) (safety 0)))
  (let ((left (%nleft node)))
    (when (setf (%nleft node) (%nright left))
      (setf (%nparent (%nright left)) node))
    (if (setf (%nparent left) (%nparent node))
      (if (eq node (%nleft (%nparent node)))
        (setf (%nleft (%nparent node)) left)
        (setf (%nright (%nparent node)) left))
      (setf (%tree-root tree) left))
    (setf (%nright left) node
          (%nparent node) left))
  (values))

(defun %insert-fixup (tree node)
  (declare (type %tree tree) (type %node node)
           (optimize (speed 3) (safety 0)))
  (labels ((grandparent (node)
             (if (and node (%nparent node))
               (%nparent (%nparent node))
               nil))
           (uncle (node &aux (g (grandparent node)))
             (cond
               ((null g) nil)
               ((eq (%nleft g) (%nparent node)) (%nright g))
               (T (%nleft g))))
           (case1 (node)
            (if (null (%nparent node))
              (setf (%nred node) nil)
              (case2 node)))
           (case2 (node)
            (when (%nred (%nparent node))
              (case3 node)))
           (case3 (node &aux (u (uncle node)) g)
            (if (and u (%nred u))
              (progn (setf (%nred (%nparent node)) nil
                           (%nred u) nil
                           g (grandparent node)
                           (%nred g) t)
                     (case1 g))
              (case4 node)))
           (case4 (node &aux (g (grandparent node)))
            (cond ((and (eq node (%nright (%nparent node)))
                        (eq (%nparent node) (%nleft g)))
                   (%rotate-left tree (%nparent node))
                   (setf node (%nleft node)))
                  ((and (eq node (%nleft (%nparent node)))
                        (eq (%nparent node) (%nright g)))
                   (%rotate-right tree (%nparent node))
                   (setf node (%nright node))))
            (case5 node))
           (case5 (node  &aux (g (grandparent node)))
            (setf (%nred (%nparent node)) nil
                  (%nred g) t)
            (if (and (eq node (%nleft (%nparent node)))
                     (eq (%nparent node) (%nleft g)))
              (%rotate-right tree g)
              (%rotate-left tree g))))
    (case1 node))
  (values))

(defun %insert-node (object tree)
  (declare (type %tree tree)
           (type standard-object object)
           (optimize (speed 3) (safety 0)))
  (if (null (%tree-root tree))
    (setf (%tree-root tree) (%node (make-weak-pointer object) (slot-value object '%hash) nil nil))
    (loop :with node :of-type %node = (%tree-root tree)
      :with code :of-type (integer 0 #.most-positive-fixnum) = (slot-value object '%hash) :do
      (cond        
        ((> code (%ncode node))
         (if (null (%nright node))
           (return
             (%insert-fixup
               tree
               (setf (%nright node)
                     (%node (make-weak-pointer object) (slot-value object '%hash) node t))))
           (setf node (%nright node))))
        ((< code (%ncode node))
         (if (null (%nleft node))
           (return
             (%insert-fixup
               tree
               (setf (%nleft node)
                     (%node (make-weak-pointer object) (slot-value object '%hash) node t))))
           (setf node (%nleft node))))
        (T (return)))))
  object)

(defun %flatten-data (tree)
  (declare (type %tree tree)
           (optimize (speed 3) (safety 0)))
  (let ((nodes '()))
    (labels ((iter (node)
               (unless (null node)
                 (let ((value (weak-pointer-value (%ndata node))))
                   (when value (push value nodes)))
                 (iter (%nleft node))
                 (iter (%nright node)))))
      (iter (%tree-root tree)))
    nodes))

(defun %clear-tree (tree)
  (declare (type %tree tree))
  (setf (%tree-root tree) nil))

