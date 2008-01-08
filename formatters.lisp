;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.

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
                  (setf (rref dict 'name)
                        (specification-name (specification-results-specification (first (specification-results-children results))))
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
            ;; it's already in dictionary form
            example))
    (copy-template *html-spec-parameter-pathname*
                   (merge-pathnames "spec.html" *html-spec-parameter-pathname*)
                   (translate-results results 0))))
