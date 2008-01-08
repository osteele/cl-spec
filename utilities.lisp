;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.

(defmacro with-elapsed-time (&body body)
  "Same as PROGN, but returns the elapsed time in seconds as a second value."
  (with-gensym t0
    `(let* ((,t0 (get-internal-real-time))
           (value (progn ,@body))
           (elapsed-time
            (coerce
             (/ (- (get-internal-real-time) ,t0) internal-time-units-per-second)
                'float)))
       (values value elapsed-time))))

(defmacro define-accumulating-method ((function-name (self type) &rest args)
                                      accumulation-construct)
  (let ((value-reader function-name)
        (child-reader (or (getf args :child-reader)
                          (concatenate-symbol type "-CHILDREN")))
        (child (gensym "child")))
    `(define-method (,function-name (,self ,type))
       (loop for ,child in (,child-reader ,self)
            ,accumulation-construct (,value-reader ,child)))))
