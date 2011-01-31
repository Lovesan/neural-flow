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

(in-package #:cl-user)

(defpackage #:neural-flow
  (:use :cl)
  (:nicknames #:nflow)
  (:export
    
    ;;neuron stuff
    #:neuron
    #:error-no-neuron
    #:neuronp
    #:neuron-name
    #:neuron-owner
    #:neuron-value
    #:update-neuron
    #:compute-dependents
    #:clear-dependents
    #:abort-flow
    #:recompute-neuron
    #:bindf
    #:muffle-signal
    #:unmuffle-signal
    #:signal-muffled-p
    
    ;;classes
    #:dataflow-class
    #:neuron-slot-definition
    #:neuron-direct-slot-definition
    #:neuron-effective-slot-definition
    #:neuron-slot-definition-neuron-class
    #:neuron-slot-definition-neuron-name
    #:dataflow-object
    
    ;;slot readers and accessors
    #:slot-neuron
    #:update-slot
    ))
