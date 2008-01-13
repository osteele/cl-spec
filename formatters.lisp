;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.


(defclass specification-formatter ()
  ())

;;;
;;; Plain Text Formatter
;;;

(defclass text-specification-formatter (specification-formatter)
  ())

(defmethod format-specification-results
    ((formatter text-specification-formatter) results
     &key (output-stream t) pathname
     &allow-other-keys)
  (format t "~%~%")
  (loop for result in (specification-results-failures results)
     for i upfrom 1
     do (with-slots (name condition) result
            (format output-stream "~D)~%~S FAILED~%~A~%~A~%~%"
                    i name condition pathname)))
  (format output-stream "Finished in ~F seconds~%~%"
          (specification-results-elapsed-time results))
  (format output-stream "~D example~:P, ~D failure~:P"
          (specification-results-examples-count results)
          (specification-results-failures-count results)))


;;;
;;; Status line formatter
;;;

(defclass status-line-specification-formatter (specification-formatter)
  ())

(defmethod format-specification-results
    ((formatter status-line-specification-formatter) results
     &key &allow-other-keys)
  (if (zerop (specification-results-failures-count results))
      (format nil "~D example~:P passed"
              (specification-results-examples-count results))
      (format nil "~D failure~:P: ~{~A~^, ~}"
              (specification-results-failures-count results)
              (loop for example in (specification-results-failures results)
                   collect (example-name example)))))


;;;
;;; HTML Formatter
;;;

(defvar *html-spec-parameter-pathname*
  (merge-pathnames "template.html" *load-pathname*)
  "The :FORMAT 'HTML option to RUN-SPECIFICATION starts with this.")

(defclass html-specification-formatter (specification-formatter)
  ())

(defmethod format-specification-results
    ((formatter html-specification-formatter) results
     &key pathname &allow-other-keys)
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
                  (setf (rref dict 'name)
                        (specification-name
                         (specification-results-specification
                          (first (specification-results-children results))))
                        (rref dict 'children)
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
                  (setf (rref dict 'name)
                        (specification-name (specification-results-specification results))
                        (rref dict 'examples)
                        (mapcar #'translate-example (specification-results-examples results)))
                  dict))))
          (translate-example (example)
            (with-slots (name success condition) example
              {name name success success condition condition})))
    (copy-template *html-spec-parameter-pathname*
                   (merge-pathnames (make-pathname :type "html") pathname)
                   (translate-results results 0))))
