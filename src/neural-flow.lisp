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

(closer-mop:defclass neuron (closer-mop:funcallable-standard-object)
  ((name :initarg :name :type symbol)
   (owner :initarg :owner)
   (dendrites :initarg :dendrites :type list)
   (value :initarg :value)
   (%function :initarg :function :type function)
   (handler :initarg :handler :type function)
   (handler-filter :initarg :handler-filter))
  (:default-initargs
      :name nil
      :owner nil
      :function (lambda (value &optional sender)
                  (declare (ignore sender))
                  value)
      :dendrites '()
      :handler (lambda (neuron condition)
                 (declare (ignore neuron condition))
                 (values))
      :handler-filter 'error)
  (:metaclass closer-mop:funcallable-standard-class))

(defun neuron-name (neuron)
  (declare (type neuron neuron))
  (slot-value neuron 'name))

(defun neuron-owner (neuron)
  (declare (type neuron neuron))
  (slot-value neuron 'owner))

(defun neuron-handler (neuron)
  (declare (type neuron neuron))
  (slot-value neuron 'handler))

(defun (setf neuron-handler) (new-handler neuron)
  (declare (type neuron neuron)
           (type (or symbol function cons) new-handler))
  (setf (slot-value neuron 'handler)
        (if (functionp new-handler)
          new-handler
          (fdefinition new-handler))))

(defun neuron-handler-filter (neuron)
  (declare (type neuron neuron))
  (copy-list (slot-value neuron 'handler-filter)))

(defun (setf neuron-handler-filter) (new-filter neuron)
  (declare (type neuron neuron))
  (let ((conditions (delete-duplicates
                      (mapcar (lambda (condition)
                                (let ((condition (if (symbolp condition)
                                                   (find-class condition)
                                                   condition)))
                                  (assert (or (eq condition (find-class t))
                                              (typep condition 'condition)
                                              (and (typep condition 'class)
                                                   (subtypep (class-name condition)
                                                             'condition)))
                                      (condition)
                                    'type-error :datum condition
                                    :expected-type '(or condition class))
                                  condition))
                        (if (listp new-filter)
                          new-filter
                          (list new-filter))))))
    (setf (slot-value neuron 'handler-filter)
          conditions)))

(defun neuron-dendrites (neuron)
  (declare (type neuron neuron))
  (copy-list (slot-value neuron 'dendrites)))

(defun neuron-function (neuron)
  (declare (type neuron neuron))  
  (slot-value neuron '%function))

(defun (setf neuron-function) (new-function neuron)
  (declare (type neuron neuron)
           (type (or symbol function cons) new-function))
  (let ((function (if (functionp new-function)
                    new-function
                    (fdefinition new-function))))
    (setf (slot-value neuron '%function) function)))

(defun %neuron-handler (neuron cond)
  (loop :for filtered-cond :in (slot-value neuron 'handler-filter)
    :when (or (eq cond filtered-cond)
              (and (typep filtered-cond 'class)
                   (typep cond (class-name filtered-cond))))
    :do (return (funcall (neuron-handler neuron) neuron cond))))

(defun neuron-value (neuron)
  (declare (type neuron neuron))
  (slot-value neuron 'value))

(closer-mop:defgeneric update-neuron (neuron)
  (:method ((neuron neuron))
    (loop :with neuron-value = (neuron-value neuron)
      :for dendrite :in (slot-value neuron 'dendrites)
      :do (funcall dendrite neuron-value neuron))))

(defun (setf neuron-value) (new-value neuron &optional sender)
  (block function
    (let ((new-value (handler-bind
                       ((condition (lambda (c) (%neuron-handler neuron c))))
                       (prog ()
                           start
                           (restart-case
                             (return (funcall (neuron-function neuron) new-value sender))
                             (retry-processing (&optional (value new-value))
                                 (setf new-value value)
                                 (go start))
                             (abort-processing (&optional return-value)
                                 (return-from function return-value)))))))
      (prog1 (setf (slot-value neuron 'value) new-value)
       (update-neuron neuron)))))

(closer-mop:defmethod shared-initialize :after
  ((object neuron) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (setf (neuron-function object) (slot-value object '%function)
        (neuron-handler object) (slot-value object 'handler)
        (neuron-handler-filter object) (slot-value object 'handler-filter))
  (closer-mop:set-funcallable-instance-function
    object (lambda (value &optional sender)
             (setf (neuron-value object sender) value))))

(closer-mop:defmethod shared-initialize :around
  ((object neuron) slot-names &rest initargs &key (value nil value-p) &allow-other-keys)
  (declare (ignore slot-names initargs))
  (multiple-value-prog1 (call-next-method)
   (when value-p (setf (neuron-value object) value))))

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
  ((neuron-class :initarg :neuron-class :reader slot-definition-neuron-class)
   (neuron-name :initarg :neuron-name :reader slot-definition-neuron-name))
  (:default-initargs :neuron-class (find-class 'neuron)))

(closer-mop:defclass neuron-direct-slot-definition
    (closer-mop:standard-direct-slot-definition
     neuron-slot-definition)
  ())

(closer-mop:defclass neuron-effective-slot-definition
    (closer-mop:standard-effective-slot-definition
     neuron-slot-definition)
  ((static-neuron :accessor slot-definition-neuron)))

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
    (setf (slot-definition-neuron slot-def)
          (let ((neuron (make-instance (slot-definition-neuron-class slot-def)
                          :name (slot-definition-neuron-name slot-def)
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
  (let* ((slot-def (call-next-method))
         (dslot-def (find-if (lambda (name)
                               (eq name (closer-mop:slot-definition-name slot-def)))
                             slot-defs
                             :key #'closer-mop:slot-definition-name)))
    (when (typep dslot-def 'neuron-direct-slot-definition)
      (setf (slot-value slot-def 'neuron-class)
            (slot-definition-neuron-class dslot-def)
            (slot-value slot-def 'neuron-name)
            (slot-definition-neuron-name dslot-def)))
    slot-def))

(closer-mop:defmethod allocate-instance :around
  ((class dataflow-class) &rest initargs)
  (declare (ignore initargs))
  (loop :with instance = (call-next-method)
    :for slot-def :in (closer-mop:class-slots class)
    :unless (eq :class (closer-mop:slot-definition-allocation slot-def))
    :do (setf (closer-mop:standard-instance-access
                instance (closer-mop:slot-definition-location slot-def))
              (make-instance (slot-definition-neuron-class slot-def)
                :name (slot-definition-neuron-name slot-def)
                :owner instance))
    :finally (return instance)))

(closer-mop:defmethod closer-mop:slot-value-using-class
    ((class dataflow-class) object (slot-def neuron-effective-slot-definition))
  (neuron-value
    (if (eq :class (closer-mop:slot-definition-allocation slot-def))
      (slot-definition-neuron slot-def)
      (closer-mop:standard-instance-access
        object
        (closer-mop:slot-definition-location slot-def)))))

(closer-mop:defmethod (setf closer-mop:slot-value-using-class)
    (new-value (class dataflow-class) object (slot-def neuron-effective-slot-definition))
  (setf (neuron-value
          (if (eq :class (closer-mop:slot-definition-allocation slot-def))
            (slot-definition-neuron slot-def)
            (closer-mop:standard-instance-access
              object
              (closer-mop:slot-definition-location slot-def)))
          object)
        new-value))

(closer-mop:defmethod closer-mop:validate-superclass
    ((class dataflow-class) (superclass standard-class))
  t)

(closer-mop:defmethod closer-mop:validate-superclass
    ((class standard-class) (superclass dataflow-class))
  t)

(closer-mop:defmethod closer-mop:slot-boundp-using-class
    ((class dataflow-class) object (slot-def neuron-effective-slot-definition))
  (let ((neuron (if (eq :class (closer-mop:slot-definition-allocation slot-def))
                  (slot-definition-neuron slot-def)
                  (closer-mop:standard-instance-access
                    object (closer-mop:slot-definition-location slot-def)))))
    (slot-boundp neuron 'value)))

(closer-mop:defmethod closer-mop:slot-makunbound-using-class
    ((class dataflow-class) object (slot-def neuron-effective-slot-definition))
  (let ((neuron (if (eq :class (closer-mop:slot-definition-allocation slot-def))
                  (slot-definition-neuron slot-def)
                  (closer-mop:standard-instance-access
                    object (closer-mop:slot-definition-location slot-def)))))
    (slot-makunbound neuron 'value)
    (reinitialize-instance neuron))
  object)

(closer-mop:defclass dataflow-object ()
  ()
  (:metaclass dataflow-class))

(defun slot-neuron (dataflow-object neuron-name)
  (declare (type dataflow-object dataflow-object)
           (type symbol neuron-name))
  (let* ((class (class-of dataflow-object))
         (slot-def (find-if (lambda (def-name)
                              (eq def-name neuron-name))
                            (closer-mop:class-slots class)
                            :key #'slot-definition-neuron-name)))
    (unless (and slot-def (typep slot-def 'neuron-slot-definition))
      (error "Object ~s has no neuron slot named ~s" dataflow-object neuron-name))
    (if (eq :class (closer-mop:slot-definition-allocation slot-def))
      (slot-definition-neuron slot-def)
      (closer-mop:standard-instance-access
        dataflow-object (closer-mop:slot-definition-location slot-def)))))

(defun slot-function (dataflow-object neuron-name)
  (declare (type dataflow-object dataflow-object)
           (type symbol neuron-name))
  (neuron-function (slot-neuron dataflow-object neuron-name)))

(defun (setf slot-function) (new-function dataflow-object neuron-name)
  (declare (type dataflow-object dataflow-object)
           (type symbol neuron-name)
           (type (or symbol function cons) new-function))
  (setf (neuron-function (slot-neuron dataflow-object neuron-name))
        new-function))

(defun update-slot (dataflow-object neuron-name)
  (declare (type dataflow-object dataflow-object)
           (type symbol neuron-name))
  (update-neuron (slot-neuron dataflow-object neuron-name)))

(defun slot-handler (dataflow-object neuron-name)
  (declare (type dataflow-object dataflow-object)
           (type symbol neuron-name))
  (neuron-handler (slot-neuron dataflow-object neuron-name)))

(defun (setf slot-handler) (new-handler dataflow-object neuron-name)
  (declare (type dataflow-object dataflow-object)
           (type symbol neuron-name)
           (type (or symbol function cons) new-handler))
  (setf (neuron-handler (slot-neuron dataflow-object neuron-name))
        new-handler))

(defun slot-handler-filter (dataflow-object neuron-name)
  (declare (type dataflow-object dataflow-object)
           (type symbol neuron-name))
  (neuron-handler-filter (slot-neuron dataflow-object neuron-name)))

(defun (setf slot-handler-filter) (new-handler-filter dataflow-object neuron-name)
  (declare (type dataflow-object dataflow-object)
           (type symbol neuron-name)
           (type (or symbol cons) new-handler-filter))
  (setf (neuron-handler-filter (slot-neuron dataflow-object neuron-name))
        new-handler-filter))

(defun add-connection (source destination &key source-name destination-name (test #'eq))
  (declare (type (or neuron dataflow-object) source)
           (type (or neuron function symbol cons dataflow-object) destination)
           (type symbol source-name destination-name)
           (type (or symbol function) test))
  (let ((source (if (typep source 'neuron)
                  source
                  (slot-neuron source source-name)))
        (destination (typecase destination
                       ((or neuron function) destination)
                       ((or symbol cons) (fdefinition destination))
                       (T (slot-neuron destination destination-name)))))
    (pushnew destination (slot-value source 'dendrites) :test test)
    (values source destination)))

(defun remove-connection (source destination &key source-name destination-name (test #'eq))
  (declare (type (or neuron dataflow-object) source)
           (type (or neuron function symbol cons dataflow-object) destination)
           (type symbol source-name destination-name)
           (type (or symbol function) test))
  (let ((source (if (typep source 'neuron)
                  source
                  (slot-neuron source source-name)))
        (destination (typecase destination
                       ((or neuron function) destination)
                       ((or symbol cons) (fdefinition destination))
                       (T (slot-neuron destination destination-name)))))
    (setf (slot-value source 'dendrites)
          (delete destination (slot-value source 'dendrites) :test test))
    (values source destination)))
