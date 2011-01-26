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

(in-package #:nflow-examples)

(closer-mop:defclass my-object (dataflow-object)
  ((%event :initform nil :neuron-name event))
  (:metaclass dataflow-class))

(defun fire-event (instance)
  (update-slot instance 'event))

(defparameter *my-object* (make-instance 'my-object))

(add-connection *my-object* (lambda (data &optional sender)
                              (declare (ignore data sender))
                              (write-line "An event was fired"))
                :source-name 'event)

(defun event-example ()
  (fire-event *my-object*))

;;(event-example)
;; ==> An event was fired

(defclass textbox (dataflow-object)
  ((text :initform "Hello, world!" :accessor textbox-text))
  (:metaclass dataflow-class))

(defparameter *textbox* (make-instance 'textbox))

(setf (slot-function *textbox* 'text)
      (lambda (data &optional sender)
        (declare (ignore sender))
        (unless (typep data 'string)
          (error "Textbox `text' slot only accepts strings"))
        data))

(defun abort-flow-handler (neuron cond)
  (declare (ignore neuron))
  (format *error-output* "~&Something gone wrong: ~a~%" cond)
  (invoke-restart 'abort-processing))

(setf (slot-handler *textbox* 'text) #'abort-flow-handler)

(defparameter *text-processor*
    (make-instance 'neuron
      :function (lambda (text &optional sender)
                  (declare (ignore sender))
                  (let* ((*read-eval* nil)
                         (data (read-from-string text)))
                    (unless (numberp data)
                      (error "Invalid input: ~s" data))
                    (cons data (* data 2))))
      :handler #'abort-flow-handler))

(add-connection *textbox* *text-processor* :source-name 'text)

(add-connection *text-processor* (lambda (data &optional sender)
                                   (declare (ignore sender))
                                   (format t "~&~a + ~:*~a = ~a~%"
                                           (car data) (cdr data))))

(defun textbox-example (&optional (text "123"))
  (setf (textbox-text *textbox*) text)
  (setf (textbox-text *textbox*) 123)
  (setf (textbox-text *textbox*) "abc"))

;;(textbox-example)
;; ==> 123 + 123 = 246
;;     ...
