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

(defun simple-pipeline ()
  (let* ((input (neuron (neuron old-value :name 'input)
                  (write-line "Input invoked")
                  (random 256)))
         (processor (neuron (neuron val :name 'processor)
                      (write-line "Processor invoked")
                      (expt (neuron-value input) 2)))
         (printer (neuron (neuron val :name 'printer)
                    (let ((value (neuron-value processor)))
                      (format t "Printer invoked: ~a" value)
                      value))))
    (list input processor printer)))

(defun muffled-example ()
  (let* ((observable (neuron (neuron value :name 'observable :value t)
                       (unless value (muffle-signal))
                       value))
         (observer (neuron (neuron value :name 'observer)
                     (format t "Value changed: ~s"
                             (neuron-value observable)))))
    (list observable observer)))
