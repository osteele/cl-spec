;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.

;;;
;;; Running
;;;

(defmethod run-specification ((self specification) &key onsuccess onerror)
  "Run all the examples.  Returns a SPECIFICATION-RESULTS.

Applies ONSUCCESS or ONERROR to each example name, depending on
whether the example passes.  Callbacks are used so that the caller can
show incremental progress during execution."
  (flet ((run-example (name fn)
           "Returns values success and condition"
           (if (specification-setup self)
               (funcall (specification-setup self)))
           (handler-case (progn
                           (funcall fn)
                           (if onsuccess
                               (funcall onsuccess name))
                           t)
             (t (condition)
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
     (nreverse *specifications*)))

;; FIXME: this shouldn't be in the same gf as the method on SPECIFICATION
(defmethod run-specification ((pathname pathname)
                              &rest args
                              &key
                              (format 'text))
  "Run the specifications in PATHNAME reporting ,results to standard output."
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

(defmethod run-specification ((string string) &rest args &key &allow-other-keys)
  (apply #'run-specification (pathname string) args))

(defmethod specification-runner ((pathname pathname) &key &allow-other-keys)
  "Run the specifications in PATHNAME reporting results to standard output."
  (run-specification pathname))

(defmethod specification-runner ((string string) &rest args &key &allow-other-keys)
  (apply #'run-specification (pathname string) args))
