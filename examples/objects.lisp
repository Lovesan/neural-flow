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

(closer-mop:defclass textbox ()
  ((text :initform "" :accessor textbox-text))
  (:metaclass dataflow-class))

(closer-mop:defclass checkbox ()
  ((checked :initform nil :accessor checkbox-checked))
  (:metaclass dataflow-class))

(defun event-example ()
  (let* ((textbox (make-instance 'textbox))
         (checkbox (make-instance 'checkbox)))
    (bindf (lambda (old-value)
             (declare (ignore old-value))
             (if (checkbox-checked checkbox)
               "Checked"
               "Unchecked"))
           textbox
           :slot 'text)
    (let ((observer (neuron (neuron val :name 'observer)
                      (format t "Text changed. New value: ~s"
                              (textbox-text textbox)))))
      (list checkbox textbox observer))))
