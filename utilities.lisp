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
