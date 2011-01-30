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

(defvar *current-neuron* nil)

(closer-mop:defclass neuron (closer-mop:funcallable-standard-object)
  ((%name :initarg :name :type symbol)
   (%owner :initarg :owner)
   (%dependents :initform '())
   (%value :initarg :function :initarg :value))
  (:default-initargs
      :name nil      
      :owner nil
      :value nil
      :function #'identity)
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod print-object ((object neuron) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (slot-value object '%name) :stream stream))
  object)

(defun neuronp (object)
  (typep object 'neuron))

(defun neuron-name (neuron)
  (declare (type neuron neuron))
  (slot-value neuron '%name))

(defun neuron-owner (neuron)
  (declare (type neuron neuron))
  (slot-value neuron '%owner))

(defun neuron-value (neuron)
  (declare (type neuron neuron))
  (when *current-neuron*
    (pushnew *current-neuron* (slot-value neuron '%dependents) :test #'eq))
  (slot-value neuron '%value))

(closer-mop:defgeneric compute-dependents (object)
  (:documentation "Computes an ordered list of object's dependents need to be updated")
  (:method ((object neuron))
    (slot-value object '%dependents)))

(closer-mop:defgeneric clear-dependents (object)
  (:documentation "Clears all of object's dependents")
  (:method ((object neuron))
    (setf (slot-value object '%dependents) '())))

(defun %sort-dependents (neuron)
  (let ((unsorted (cons (list neuron) nil)))
    (setf (cdr unsorted) (car unsorted))
    (loop :until (null (car unsorted)) :collect
      (let ((next (pop (car unsorted))))
        (loop :for node :in (compute-dependents next)
          :for cell = (cons node nil)
          :do (setf (cddr unsorted) cell
                    (cdr unsorted) cell
                    (car unsorted) (or (car unsorted) cell)))
        (clear-dependents next)
        next))))

(defun update-neuron (neuron)
  (declare (type neuron neuron))
  (loop :for *current-neuron* :in (%sort-dependents neuron)
    :do (tagbody
          compute-value
          (restart-case
            (setf (slot-value *current-neuron* '%value)
                  (funcall *current-neuron* (slot-value *current-neuron* '%value)))
            (recompute-neuron (&optional (value (slot-value *current-neuron* '%value)))
              (setf (slot-value *current-neuron* '%value) value)
              (go compute-value))
            (abort-flow (&optional (return-value (slot-value neuron '%value)))
              (return (setf (slot-value neuron '%value) return-value)))))
    :finally (return (slot-value *current-neuron* '%value))))

(defun (setf neuron-value) (new-value neuron)
  (declare (type neuron neuron))
  (prog1 (setf (slot-value neuron '%value) new-value)
   (update-neuron neuron)))

(closer-mop:defgeneric bindf (function object &key &allow-other-keys)
  (:documentation "Binds specified computation to an object"))

(closer-mop:defmethod bindf ((function function) (object neuron) &key &allow-other-keys)
  (closer-mop:set-funcallable-instance-function object function)
  (update-neuron object)
  function)

(closer-mop:defmethod shared-initialize :around
  ((object neuron) slot-names &rest initargs
   &key (function #'identity)
   &allow-other-keys)
  (declare (ignorable slot-names initargs))
  (check-type function (or symbol function))  
  (remf initargs :function)
  (prog1 (apply #'call-next-method object slot-names initargs)
   (bindf (if (functionp function)
            function
            (fdefinition function))
          object)))

(defmacro neuron ((&optional (neuron-class 'neuron) (value-var (gensym)) &rest args)
                  &body body)
  (check-type value-var symbol)
  `(make-instance ',neuron-class
     :function (lambda (,value-var) (declare (ignorable ,value-var)) ,@body)
     ,@args))
