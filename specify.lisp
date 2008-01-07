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

;; The SPECIFICATION class.
(define-class specification name
  (setup nil (or function null)) ; a function to run before each example
  (examples nil (list-type (cons string function))))

(define-print-method (specification name) "#<specification ~S>" name)

(define-method (run-specification (self specification) &key onsuccess onerror)
  "Test all the examples, and returns a list of dictionaries {:name, :success, :condition, :time}.

Applies ONSUCCESS or ONERROR to each one, depending on whether it
passes.  Callbacks are used so that the caller can show incremental
progress during execution."
  (flet ((run-example (name fn)
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
               nil))))
    (loop for (name . fn) in (specification-examples self)
       collect (cons name (run-example name fn)))))

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

(defmacro with-collecting-specifications (&body body)
  `(let ((*collect-specifications* t)
         (*run-specifications* nil)
         (*specifications* nil))
     ,@body
     *specifications*))

(defmacro with-elapsed-time (&body body)
  (with-gensym t0
    `(let ((,t0 (get-internal-real-time)))
       ,@body
       (/ (- (get-internal-real-time) ,t0) internal-time-units-per-second))))

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

(define-method (specification-runner (pathname pathname) &key &allow-other-keys)
  "Run the specifications in PATHNAME reporting results to standard output."
  (run-specification pathname))

(define-method (specification-runner (string string) &key &allow-other-keys)
  (run-specification (pathname string)))