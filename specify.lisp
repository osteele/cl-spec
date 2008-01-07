(define-class specification name setup examples)

(define-print-method (specification name) "#<specification ~S>" name)

(define-method (run-specification (self specification) &key onsuccess onerror)
  (flet ((run-example (name fn)
           (if (specification-setup self)
               (funcall (specification-setup self)))
           (handler-case (progn
                           (funcall fn)
                           (if onsuccess
                               (funcall onsuccess name))
                           t)
             (expectation-not-met (condition)
               ;(format t "~A while executing ~A" condition name)
               (if onerror
                   (funcall onerror name condition))
               nil))))
    (loop for (name . fn) in (specification-examples self)
       collect (cons name (run-example name fn)))))

(defvar *collect-specifications* nil)
(defvar *run-specifications* t)
(defvar *specifications* nil)

(defmacro define-specification (name values &body body)
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

(define-method (run-specification (pathname pathname) &key onsuccess onerror)
  (declare (ignore onsuccess onerror))
  (let ((*collect-specifications* t)
        (*run-specifications* nil)
        (*specifications* nil)
        (t0 (get-internal-real-time))
        (example-count 0)
        (failures nil)
        elapsed-time)
    (load pathname)
    (for spec in *specifications*
      do (incf example-count (length (specification-examples spec)))
      do (run-specification spec
                            :onsuccess #'(lambda (name)
                                           (declare (ignore name))
                                           (format t "."))
                            :onerror #'(lambda (name condition)
                                         (push `(,name . ,condition) failures)
                                         (format t "F"))))
    (setf elapsed-time (- (get-internal-real-time) t0))
    (format t "~%~%")
    ;(print failures)
    (loop for (name . condition) in failures
         for i upfrom 1
         do (format t "~D)~%~A~%~A~%~%" i condition pathname))
    (format t "Finished in ~F seconds~%~%"
            (/ elapsed-time INTERNAL-TIME-UNITS-PER-SECOND))
    (format t "~D example~:P, ~D failure~:P" example-count (length failures))
    (values)))
