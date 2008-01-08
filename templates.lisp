;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.

;; A quick and dirty template facilityq that does just enough to
;; enable HTML output from cl-spec.  A proper implementation would
;; compile to a function, as well as use a proper parser.

(defun copy-template (source-pathname target-pathname dictionary)
  "Copy the contents of SOURCE-PATHNAME to TARGET-PATHNAME, interpolating
${expr} constructs against the environment in DICTIONARY."
  (let* ((template (read-template source-pathname)))
    (with-open-file (s target-pathname :direction :output :if-exists :supersede)
      (apply-template template dictionary s)
      target-pathname)))

;; TODO: this could be a LOT more efficient
(defmacro with-next-substring ((string &optional (min (gensym "min")))
                               &body clauses)
  ;; not all positions are used, but it's easier to keep this the same
  ;; length as clauses
  (let ((positions (loop for (token . body) in clauses
                        if (stringp token)
                      collect (gensym)
                      else
                      collect nil))
        (occurrences (gensym "occurrences")))
    `(let* (,@(loop for (token . body) in clauses
                for pos in positions
                if (stringp token)
                collect `(,pos (search ,token ,string)))
           (,occurrences (delete nil (list ,@(remove nil positions))))
             (,min (if ,occurrences (apply #'min ,occurrences))))
       (cond ,@(loop for (token . body) in clauses
                    for pos in positions
                    if (stringp token)
                    collect `((and ,min (eql ,min ,pos)) ,@body)
                    else
                    collect `(,token ,@body))))))

;; FIXME: doesn't know to avoid punctuation in strings
(defun read-template (pathname)
    (let ((stack nil)
          (chunks nil)
          context)
      ;; Hand-crafted state-machine parser.  Run away!  Run away!
      ;; (More realistically, find a version of yacc or ragel that's
      ;; not too heavy-weight.)
      (labels ((process-line (line &optional (crlf t))
                 (with-next-substring (line pos)
                   ((not pos)
                    (literal line crlf))
                   ((< 0 pos)
                    (process-line (subseq line 0 pos) nil)
                    (process-line (subseq line pos) crlf))
                   ("${"
                    (begin-interpolation (subseq line 2) crlf))
                   ("$}"
                    (end-iteration)
                    (process-line (subseq line 2) crlf))))
               (literal (string crlf)
                 (unless (string= string "")
                   (push string chunks))
                 (if crlf
                     (push #\newline chunks)))
               (begin-interpolation (string crlf)
                 (with-next-substring (string pos)
                   ("}"
                    (compile-interpolation (subseq string 0 pos))
                    (process-line (subseq string (1+ pos)) crlf))
                   (t
                    (begin-iteration string crlf))))
               (compile-interpolation (string)
                 (with-next-substring (string pos)
                   ("|"
                    (push {:type :format
                          :format-string
                          (trim (subseq string 0 pos))
                          :format-args
                          (loop for symbol in (split (subseq string (1+ pos)) #\space)
                             unless (string= "" symbol)
                             collect (intern (string-upcase (trim symbol))))
                          }
                          chunks))
                   (t
                    (push (intern (string-upcase string)) chunks))))
               (begin-iteration (string crlf)
                 (with-next-substring (string pos)
                   ("=>"
                    (let ((variable (intern (string-upcase (trim (subseq string 0 pos)))))
                          (residue (subseq string (+ pos 2))))
                      (push (cons chunks context) stack)
                      (setf chunks nil
                            context {:type :iteration :sequence-variable variable}
)
                      (process-line residue crlf)))
                   (t
                    (error "unrecognized interpolation format: ~S" string))))
               (end-iteration ()
                 (let ((iterator context))
                   (destructuring-bind (previous-chunks . previous-context)
                       (pop stack)
                     (setref context :body (nreverse chunks))
                     (setf chunks previous-chunks
                           context previous-context)
                     (push iterator chunks)))))
        (with-open-file (s pathname :direction :input)
          (map-lines #'process-line s)))
      (nreverse chunks))
    )

(defvar *trace-templates* nil)

(defun apply-template (template &optional (dictionary {}) (output-stream t))
  (if (stringp template)
      (setf template (read-template template)))
  (labels ((lookup (key)
             ;; FIXME: kludge
             (unless (has-key-p dictionary key)
               (setf key (intern (symbol-name key) :keyword)))
             (assert (has-key-p dictionary key) (key)
                     "The environment does not contain ~S (keys = ~S)"
                     key (keys dictionary))
             (ref1 dictionary key)))
    (dolist (chunk template)
      (flet ((field (key)
               (assert (has-key-p chunk key) (key)
                       "The context does not have a field named ~S (keys = ~S)"
                       key (keys chunk))
               (ref1 chunk key)))
        (typecase chunk
          ((or string character)
           (princ chunk output-stream))
          (symbol
           (when *trace-templates*
             (format t "looking up ~S -> ~S~%"
                     chunk (ref1 dictionary chunk)))
           (princ (lookup chunk) output-stream))
          (t
           (case (field :type)
             (:format
              (when *trace-templates*
                (format t "formatting ~S~%" (field :format-args)))
              (let ((format-string (field :format-string))
                    (format-args (mapcar #'lookup (field :format-args))))
                (apply #'format output-stream format-string format-args)))
             (:iteration
              (let ((sequence (lookup (field :sequence-variable)))
                    (body (field :body)))
                (when *trace-templates*
                  (format t "iterating over ~S~%" sequence))
                (dolist (item sequence)
                  (apply-template body item output-stream))))
             (t
              (error "don't know that format")))))))))
