(define-class specification name setup tests)

(define-print-method (specification name) "#<specification ~S>" name)

(define-method (run (self specification))
  (flet ((run-example (name fn)
           (if (specification-setup self)
               (funcall (specification-setup self)))
           (handler-case (funcall fn)
             (expectation-not-met (condition)
               (format t "~A while executing ~A" condition name)))))
    (loop for (name . fn) in (specification-tests self)
       do (run-example name fn))))

;; TODO: run initializers on preamble
(defmacro define-specification (name values &body body)
  (let* ((before nil)
         (specs
          (loop for item in body
             if (stringp (first item))
             collect item
             else if (and (eq (car item) 'before) (eq (cadr item) :each))
             do (setf before (cddr item))))
         (tests
          (loop for item in specs
             collect ``(,,(car item) . ,#'(lambda () ,@(cdr item))))))
    `(let ,values
       (let ((spec
              (make-instance 'specification
                             :name ,name
                             :setup #'(lambda() ,@before)
                             :tests (list ,@tests))))
         (run spec)
         (setq *spec* spec)
         spec))))
