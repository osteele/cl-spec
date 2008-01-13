;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:cl-spec-asd (:use :cl :asdf))
(in-package #:cl-spec-asd)

(defsystem "cl-spec"
    :description "Behavioral Testing for Common Lisp."
    :version "0.1"
    :author "Oliver Steele <steele@osteele.com>"
    :licence "MIT License"
    :components ((:file "formatters"
                        :depends-on ("specify" "utilities" "templates"))
                 (:file "runners")
                 (:file "specify" :depends-on ("bdd" "utilities"))
                 (:file "templates"); :depends-on ("utilities"))
                 (:file "utilities")
                 (:file "bdd"))
    ;:depends-on (:lexicons)
    )
