;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.

(require 'dictionary)
(require 'bdd)

(defvar *collect-specifications* nil
  "If true, DEFINE-SPECIFICATION collects specifications into
*SPECIFICATIONS*.  (RUN-SPECIFICATION PATHNAME) binds this to true.")

(defvar *run-specifications* t
  "If true, DEFINE-SPECIFICATION runs each specification as it defines it.
This makes it useful to evaluate a DEFINE-SPECIFICATION form interactively,
as a quick check.  (RUN-SPECIFICATION PATHNAME) binds this to false,
since it runs the specifications once it has collected them all.")

(defvar *specifications* nil
  "(RUN-SPECIFICATION PATHNAME) binds this to a list of specifications
for DEFINE-SPECIFICATION to collect into.")

(defclass specification ()
  ((name :initarg :name :reader specification-name :type string)
   (setup :initarg :setup :reader specification-setup :initform nil
          :type (or function null)
          :documentation "Run this before each example")
   (examples :initarg :examples :reader specification-examples
             :type (list-type (cons string function))))
  (:documentation "DEFINE-SPECIFICATION instantiates this.  It holds a list
of named examples."))

(defmethod print-object ((self specification) stream)
  (format stream "#<specification ~S>" (specification-name name)))

(defmacro define-specification (name bindings &body body)
  "Define a SPECIFICATION, with a name, variables, and a list of examples.
See the documentation for Ruby rspec or JavaScript jsspec for an
overview on how this works; see the examples in the examples/
subdirectory for examples in Lisp syntax."
  (let* ((before nil)
         (variables
          (loop for binding in bindings
               if (symbolp binding)
               collect binding
               else collect (car binding)
               and do (setf before (nconc before `((setq ,(car binding) ,(cadr binding)))))))
         (specs
          (loop for item in body
             if (stringp (first item))
             collect item
             else if (and (eq (car item) 'before) (eq (cadr item) :each))
             do (setf before (nconc before (cddr item)))))
         (examples
          (loop for item in specs
             collect ``(,,(car item) . ,#'(lambda () ,@(cdr item))))))
    `(let ,variables
       (let ((spec
              (make-instance 'specification
                             :name ,name
                             :setup #'(lambda() ,@before)
                             :examples (list ,@examples))))
         (if *collect-specifications*
             (push spec *specifications*))
         (if *run-specifications*
             ;; quick summary for the status line:
             (format-specification-results
              (make-instance 'status-line-specification-formatter)
              (run-specification spec))
             spec)))))


;;;
;;; Specification results
;;;

(defclass example-result ()
  ((name :initarg :name :reader example-name :type string)
   (success :initarg :success :reader example-success :type boolean)
   (condition :initarg :condition :reader example-condition)))

(defclass abstract-specification-results ()
  ())

(defclass specification-results-group (abstract-specification-results)
  ((children :initarg :children
             :reader specification-results-children
             :type (list-type abstract-specification-results))))

;; these are naive implementations which wouldn't work for large sets,
;; but they work fine for now.

(define-accumulating-method (specification-results-examples
                             (self specification-results-group))
    append
  :child-reader specification-results-children)

(define-accumulating-method (specification-results-failures
                             (self specification-results-group))
    append
  :child-reader specification-results-children)

(define-accumulating-method (specification-results-elapsed-time
                             (self specification-results-group))
    sum
  :child-reader specification-results-children)

(define-accumulating-method (specification-results-examples-count
                             (self specification-results-group))
    sum
  :child-reader specification-results-children)

(define-accumulating-method (specification-results-failures-count
                             (self specification-results-group))
    sum
  :child-reader specification-results-children)


;; TODO: reify example result's type?
(defclass specification-results (abstract-specification-results)
  ((specification :initarg :specification
                  :reader specification-results-specification
                  :type specification)
   (elapsed-time :initarg :elapsed-time
                 :reader specification-results-elapsed-time)
   (examples :initarg :examples :reader specification-results-examples)))

(defmethod specification-results-failures ((self specification-results))
  (remove-if #'example-success (specification-results-examples self)))

(defmethod specification-results-failures-count ((self specification-results))
  (length (specification-results-failures self)))

(defmethod specification-results-examples-count ((self specification-results))
  (length (specification-results-examples self)))

;(define-accumulating-method (specification-result-elapsed-time
;                             (results specification-results)
;                             :child-reader specification-results-examples)
;    sum)
