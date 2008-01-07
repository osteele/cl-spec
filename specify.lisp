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

;; TODO: reify result's type?
;; TODO: separate classes for container (<-children) and leaf (<-spec and results)
(define-class specification-results specification
  (elapsed-time 0 number)             ; in seconds
  (children nil list)
  (results nil list))


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
                     :results results))))


(defmacro with-collecting-specifications (&body body)
  `(let ((*collect-specifications* t)
         (*run-specifications* nil)
         (*specifications* nil))
     ,@body
     *specifications*))

(define-method (run-specification (pathname pathname) &key onsuccess onerror)
  "Run the specifications in PATHNAME reporting results to standard output."
  (declare (ignore onsuccess onerror))
  (flet ((write-progress-char (char)
           (format t char)
           (force-output)))
    (let* ((specifications
            (with-collecting-specifications
              (load pathname)))
    (example-count 0)
    (failures nil)
    (elapsed-time
     (with-elapsed-time
       (for spec in specifications
         do (incf example-count (length (specification-examples spec)))
         do (run-specification spec
                               :onsuccess #'(lambda (name)
                                              (declare (ignore name))
                                              (write-progress-char "."))
                               :onerror #'(lambda (name condition)
                                            (push `(,name . ,condition) failures)
                                            (write-progress-char "F")))))))
      (format t "~%~%")
      (loop for (name . condition) in failures
         for i upfrom 1
         do (format t "~D)~%~A~%~A~%~%" i condition pathname))
      (format t "Finished in ~F seconds~%~%" elapsed-time)
      (format t "~D example~:P, ~D failure~:P" example-count (length failures))
      nil)))

(define-method (run-specification (string string) &rest args &key onsuccess onerror)
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
                &key (output-stream t)
                &allow-other-keys)
  )
