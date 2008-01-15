;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.

(in-package #:common-lisp-user)

(defpackage #:cl-spec
  (:use #:common-lisp)
  (:nicknames :clspec)
  (:documentation "Behavior Testing for Common Lisp.")
  (:export define-specification
           specify
           run-specification
           run-spec
           ))