#|
Copyright 2008 by Oliver Steele.  All rights reserved.

This code is available under the MIT License.
|#

(defpackage #:cl-spec-asd (:use #:cl #:asdf))
(in-package #:cl-spec-asd)

(asdf:defsystem "cl-spec"
    :description "Behavioral Testing for Common Lisp."
    :version "0.1.6"
    :author "Oliver Steele <steele@osteele.com>, Yurii Rashkovskii"
    :maintainer "Oliver Steele <steele@osteele.com>"
    :licence "MIT License"
    :serial t
    :components ((:file "package")
                 (:file "utilities")
                 (:file "templates"); :depends-on ("utilities"))
                 (:file "bdd")
                 (:file "specify" :depends-on ("bdd" "utilities"))
                 (:file "formatters"
                        :depends-on ("specify" "utilities" "templates"))
                 (:file "runners"))
    ;:depends-on (:lexicons)
    )
