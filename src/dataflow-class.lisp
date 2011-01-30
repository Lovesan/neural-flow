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

(closer-mop:defclass dataflow-class (closer-mop:standard-class)
  ()
  (:metaclass closer-mop:standard-class))

(defvar *current-class* nil)

(closer-mop:defmethod shared-initialize :after
  ((class dataflow-class) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (closer-mop:finalize-inheritance class))

(closer-mop:defmethod shared-initialize :around
  ((class dataflow-class) slot-names &rest initargs)
  (declare (ignorable slot-names initargs))
  (let ((*current-class* class))
    (call-next-method)))

(closer-mop:defclass neuron-slot-definition
    (closer-mop:standard-slot-definition)
  ((neuron-class :initarg :neuron-class :reader neuron-slot-definition-neuron-class)
   (neuron-name :initarg :neuron-name :reader neuron-slot-definition-neuron-name))
  (:default-initargs :neuron-class (find-class 'neuron)))

(closer-mop:defclass neuron-direct-slot-definition
    (closer-mop:standard-direct-slot-definition
     neuron-slot-definition)
  ())

(closer-mop:defclass neuron-effective-slot-definition
    (closer-mop:standard-effective-slot-definition
     neuron-slot-definition)
  ((static-neuron :accessor neuron-slot-definition-neuron)))

(closer-mop:defmethod initialize-instance :after
  ((slot-def neuron-slot-definition) &rest initargs
   &key neuron-class (neuron-name nil neuron-name-p) &allow-other-keys)  
  (declare (ignore initargs))
  (let ((class (etypecase neuron-class
                 (symbol (find-class neuron-class))
                 (class neuron-class))))
    (assert (subtypep (class-name class) 'neuron) (class)
      "~s is not a neuron class" class)
    (setf (slot-value slot-def 'neuron-class)
          class
          (slot-value slot-def 'neuron-name)
          (if neuron-name-p neuron-name (closer-mop:slot-definition-name slot-def)))))

(closer-mop:defmethod initialize-instance :after
  ((slot-def neuron-effective-slot-definition)
   &rest initargs &key allocation &allow-other-keys)
  (declare (ignore initargs))
  (when (eq :class allocation)
    (setf (neuron-slot-definition-neuron slot-def)
          (let ((neuron (make-instance (neuron-slot-definition-neuron-class slot-def)
                          :name (neuron-slot-definition-neuron-name slot-def)
                          :owner *current-class*)))
            (when (closer-mop:slot-definition-initfunction slot-def)
              (setf (neuron-value neuron)
                    (funcall (closer-mop:slot-definition-initfunction slot-def))))
            neuron))))

(closer-mop:defmethod closer-mop:direct-slot-definition-class
    ((class dataflow-class) &rest initargs)
  (declare (ignorable initargs))
  (find-class 'neuron-direct-slot-definition))

(closer-mop:defmethod closer-mop:effective-slot-definition-class
    ((class dataflow-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'neuron-effective-slot-definition))

(closer-mop:defmethod closer-mop:compute-effective-slot-definition :around
    ((class dataflow-class) name slot-defs)
  (declare (ignorable name))
  (let* ((slot-def (call-next-method))
         (dslot-def (find-if (lambda (name)
                               (eq name (closer-mop:slot-definition-name slot-def)))
                             slot-defs
                             :key #'closer-mop:slot-definition-name)))
    (when (typep dslot-def 'neuron-direct-slot-definition)
      (setf (slot-value slot-def 'neuron-class)
            (neuron-slot-definition-neuron-class dslot-def)
            (slot-value slot-def 'neuron-name)
            (neuron-slot-definition-neuron-name dslot-def)))
    slot-def))

(closer-mop:defmethod allocate-instance :around
  ((class dataflow-class) &rest initargs)
  (declare (ignore initargs))
  (loop :with instance = (call-next-method)
    :for slot-def :in (closer-mop:class-slots class)
    :unless (eq :class (closer-mop:slot-definition-allocation slot-def))
    :do (setf (closer-mop:standard-instance-access
                instance (closer-mop:slot-definition-location slot-def))
              (make-instance (neuron-slot-definition-neuron-class slot-def)
                :name (neuron-slot-definition-neuron-name slot-def)
                :owner instance
                :value (funcall (closer-mop:slot-definition-initfunction slot-def))))
    :finally (return instance)))

(defun %slot-neuron (object slot-def)
  (if (eq :class (closer-mop:slot-definition-allocation slot-def))
    (neuron-slot-definition-neuron slot-def)
    (closer-mop:standard-instance-access
      object
      (closer-mop:slot-definition-location slot-def))))

(closer-mop:defmethod closer-mop:slot-value-using-class
    ((class dataflow-class) object (slot-def neuron-effective-slot-definition))
  (neuron-value (%slot-neuron object slot-def)))

(closer-mop:defmethod (setf closer-mop:slot-value-using-class)
    (new-value (class dataflow-class) object (slot-def neuron-effective-slot-definition))
  (setf (neuron-value (%slot-neuron object slot-def)) new-value))

(closer-mop:defmethod closer-mop:validate-superclass
    ((class dataflow-class) (superclass standard-class))
  t)

(closer-mop:defmethod closer-mop:validate-superclass
    ((class standard-class) (superclass dataflow-class))
  t)

(closer-mop:defmethod closer-mop:slot-boundp-using-class
    ((class dataflow-class) object (slot-def neuron-effective-slot-definition))
  (declare (ignore object))
  T)

(closer-mop:defmethod closer-mop:slot-makunbound-using-class
    ((class dataflow-class) object (slot-def neuron-effective-slot-definition))
  (reinitialize-instance (%slot-neuron object slot-def))
  object)

(closer-mop:defclass dataflow-object ()
  ()
  (:metaclass dataflow-class))

(defun slot-neuron (dataflow-object neuron-name)
  (declare (type dataflow-object dataflow-object)
           (type symbol neuron-name))
  (let* ((class (class-of dataflow-object))
         (slot-def (progn
                     (unless (typep class 'dataflow-class)
                       (error "Class ~s of object ~s is not a ~s"
                              class dataflow-object 'dataflow-class))
                     (find-if (lambda (def-name) (eq def-name neuron-name))
                              (closer-mop:class-slots class)
                              :key #'neuron-slot-definition-neuron-name))))
    (unless (typep slot-def 'neuron-slot-definition)
      (error "Object ~s has no neuron slot named ~s" dataflow-object neuron-name))
    (%slot-neuron dataflow-object slot-def)))

(defun update-slot (dataflow-object neuron-name)
  (declare (type dataflow-object dataflow-object)
           (type symbol neuron-name))
  (update-neuron (slot-neuron dataflow-object neuron-name)))

(defmethod bindf ((function function) (object dataflow-object)
                  &key (slot (error "Please supply slot name")))
  (bindf function (slot-neuron object slot)))

(defmethod bindf ((function neuron) (object dataflow-object)
                  &key (slot (error "Please supply slot name")))
  (check-type slot symbol)
  (let* ((class (class-of object))
         (slot-def (progn
                     (unless (typep class 'dataflow-class)
                       (error "Class ~s of object ~s is not a ~s"
                              class object 'dataflow-class))
                     (find-if (lambda (def-name) (eq def-name slot))
                              (closer-mop:class-slots class)
                              :key #'neuron-slot-definition-neuron-name))))
    (unless (typep slot-def 'neuron-slot-definition)
      (error "Object ~s has no neuron slot named ~s" object slot))
    (if (eq :class (closer-mop:slot-definition-allocation slot-def))
      (setf (neuron-slot-definition-neuron slot-def) function)
      (setf (closer-mop:standard-instance-access
              object
              (closer-mop:slot-definition-location slot-def))
            function))
    (setf (slot-value function '%name) (neuron-slot-definition-neuron-name slot-def)
          (slot-value function '%owner) object)
    (update-neuron function)
    function))
