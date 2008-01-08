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

;; this doesn't really test that every element has type TYPE, but
;; I don't think there's a way to do that in CL
(deftype list-type (&optional (type t))
  `(and list (or null (cons ,type))))

;; DEFINE-SPECIFICATION instantiates this.  It holds a list of named
;; examples.
(define-class specification name
  (setup nil (or function null)) ; a function to run before each example
  (examples nil (list-type (cons string function))))

(define-print-method (specification name) "#<specification ~S>" name)

(defmacro define-specification (name values &body body)
  "Define a SPECIFICATION, with a name, variables, and a list of examples.
See the documentation for Ruby rspec or JavaScript jsspec for an
overview on how this works; see the examples in the examples/
subdirectory for examples in Lisp syntax."
  (let* ((before nil)
         (specs
          (loop for item in body
             if (stringp (first item))
             collect item
             else if (and (eq (car item) 'before) (eq (cadr item) :each))
             do (setf before (cddr item))))
         (examples
          (loop for item in specs
             collect ``(,,(car item) . ,#'(lambda () ,@(cdr item))))))
    `(let ,values
       (let ((spec
              (make-instance 'specification
                             :name ,name
                             :setup #'(lambda() ,@before)
                             :examples (list ,@examples))))
         (if *collect-specifications*
             (push spec *specifications*))
         (if *run-specifications*
             (run-specification spec)
             spec)))))


;;;
;;; Specification results
;;;

(define-class abstract-specification-results)

(defclass specification-results-group (abstract-specification-results)
  ((children :initarg :children
             :reader specification-results-children
             :type (list-type abstract-specification-results))))

;; these are naive implementations which wouldn't work for large sets,
;; but they work fine for now.

(define-accumulating-method (specification-results-examples
                             (self abstract-specification-results)
                             :child-reader specification-results-children)
    append)

(define-accumulating-method (specification-results-failures
                             (self abstract-specification-results)
                             :child-reader specification-results-children)
    append)

(define-accumulating-method (specification-results-elapsed-time
                             (self abstract-specification-results)
                             :child-reader specification-results-children)
    sum)

(define-method (specification-results-examples-count
                (self abstract-specification-results))
  (length (specification-results-examples self)))

(define-method (specification-results-failures-count
                (self abstract-specification-results))
  (length (specification-results-failures self)))


;; TODO: reify example's type?
(defclass specification-results (abstract-specification-results)
  ((specification :initarg :specification
                  :reader specification-results-specification
                  :type specification)
   (elapsed-time :initarg :elapsed-time
                 :reader specification-results-elapsed-time)
   (examples :initarg :examples :reader specification-results-examples)))

(define-method (specification-results-failures (self specification-results))
  (loop for example in (specification-results-examples self)
     unless (ref1 example :success)
     collect example))

;(define-accumulating-method (specification-result-elapsed-time
;                             (results specification-results)
;                             :child-reader specification-results-examples)
;    sum)


;;;
;;; Running
;;;

(define-method (run-specification (self specification) &key onsuccess onerror)
  "Test all the examples, and returns a list of dictionaries {:name, :success, :condition, :time}.

Applies ONSUCCESS or ONERROR to each one, depending on whether it
passes.  Callbacks are used so that the caller can show incremental
progress during execution."
  (flet ((run-example (name fn)
           "Returns values success and condition"
           (if (specification-setup self)
               (funcall (specification-setup self)))
           (handler-case (progn
                           (funcall fn)
                           (if onsuccess
                               (funcall onsuccess name))
                           t)
             (expectation-not-met (condition)
               (if onerror
                   (funcall onerror name condition))
               (values nil condition)))))
    (multiple-value-bind (results elapsed-time)
        (with-elapsed-time
          (loop for (name . fn) in (specification-examples self)
             collect (multiple-value-bind (success condition)
                         (run-example name fn)
                         {:name name :success success :condition condition})))
      (make-instance 'specification-results
                     :specification self
                     :elapsed-time elapsed-time
                     :examples results))))


(defmacro with-collecting-specifications (&body body)
  `(let ((*collect-specifications* t)
         (*run-specifications* nil)
         (*specifications* nil))
     ,@body
     *specifications*))

;; FIXME: this shouldn't be in the same gf as the method on SPECIFICATION
(define-method (run-specification (pathname pathname)
                                  &rest args
                                  &key
                                  (format 'text))
  "Run the specifications in PATHNAME reporting results to standard output."
  (labels ((write-progress-char (char)
             (format t char)
             (force-output))
           (note-success (&rest rest)
             (declare (ignore rest))
             (write-progress-char "."))
           (note-failure (&rest rest)
             (declare (ignore rest))
             (write-progress-char "F")))
    (let* ((formatter-class
            (concatenate-symbol format "-SPECIFICATION-FORMATTER"))
           (specifications
            (with-collecting-specifications
              (load pathname)))
           (results-children
            (loop for spec in specifications
               collect (run-specification spec
                                     :onsuccess #'note-success
                                     :onerror #'note-failure)))
           (results
            (make-instance 'specification-results-group
                           :children results-children)))
      (apply #'format-specification-results
       (make-instance formatter-class)
       results
       :pathname pathname
       args))))

(define-method (run-specification (string string) &rest args &key &allow-other-keys)
  (apply #'run-specification (pathname string) args))

(define-method (specification-runner (pathname pathname) &key &allow-other-keys)
  "Run the specifications in PATHNAME reporting results to standard output."
  (run-specification pathname))

(define-method (specification-runner (string string) &rest args &key &allow-other-keys)
  (apply #'run-specification (pathname string) args))


;;;
;;; Formatters
;;;

(define-class specification-formatter)
(define-class text-specification-formatter (specification-formatter))

(define-method (format-specification-results
                (formatter text-specification-formatter)
                results
                &key (output-stream t) pathname
                &allow-other-keys)
  (format t "~%~%")
  (loop for result in (specification-results-failures results)
     for i upfrom 1
     do (format output-stream "~D)~%~A~%~A~%~%" i (ref1 result :condition) pathname))
  (format output-stream "Finished in ~F seconds~%~%"
          (specification-results-elapsed-time results))
  (format output-stream "~D example~:P, ~D failure~:P"
          (specification-results-examples-count results)
          (specification-results-failures-count results)))

(define-class html-specification-formatter (specification-formatter))

(defvar *html-spec-parameter-pathname*
  (merge-pathnames "template.html" *load-pathname*)
  "The :FORMAT 'HTML option to RUN-SPECIFICATION starts with this.")

(define-method (format-specification-results
                (formatter html-specification-formatter)
                results
                &key
                &allow-other-keys)
  ;; for now, the group hierarchy must be exactly one deep
  (labels ((translate-results (results depth)
             (etypecase results
               (specification-results-group
                (assert (= depth 0) () "for now, groups can't be nested")
                ;; TODO: would be nicer with a general serialization
                ;; mechanism instead of adding keys afterwards; or
                ;; else maybe the templater should use accessors
                ;; instead of dictionary conversion
                (let ((dict
                       (object->dictionary results
                                          '(examples-count
                                            failures-count
                                            elapsed-time)
                                          :basename 'specification-results)))
                  (setref dict 'name
                          (specification-name (specification-results-specification (first (specification-results-children results)))))
                  (setref dict 'children
                          (loop for child in (specification-results-children results)
                             collect (translate-results child (1+ depth))))
                  dict))
               (specification-results
                (assert (= depth 1) ()
                        "for now, specification result leaves must be exactly one deep")
                (let ((dict
                       (object->dictionary results '(examples-count
                                                     failures-count
                                                     elapsed-time)
                                           :basename 'specification-results)))
                  (setref dict 'name
                          (specification-name (specification-results-specification results)))
                  (setref dict 'examples
                          (mapcar #'translate-example (specification-results-examples results)))))))
          (translate-example (example)
            ;; it's already in dictionary form
            example))
    (copy-template *html-spec-parameter-pathname*
                   (merge-pathnames "spec.html" *html-spec-parameter-pathname*)
                   (translate-results results 0))))
