;; This is a quick and dirty hack that does just enough to make html
;; output from cl-spec easy.  It should

(defun copy-template (source-pathname target-pathname dictionary)
  (let* ((template (read-template source-pathname)))
    (with-open-file (s target-pathname :direction :output :if-exists :supersede)
      (apply-template template dictionary s))))

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

;; note: doesn't know to avoid punctuation in strings
(defun read-template (pathname)
    (let ((stack nil)
          (chunks nil)
          (current nil))
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
                   ("}"
                    (end-iteration)
                    (process-line (subseq line 1) crlf))))
               (literal (string crlf)
                 (push string chunks)
                 (if crlf
                     (push #.(format nil "~%") chunks)))
               (begin-interpolation (string crlf)
                 (with-next-substring (string pos)
                   ("}"
                    (interpoland (subseq string 0 pos))
                    (process-line (subseq string (1+ pos)) crlf))
                   (t
                    (begin-iteration string))))
               (interpoland (string)
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
                    (push (intern (string-upcase string)) chunks)))))
        (with-open-file (s pathname :direction :input)
          (map-lines #'process-line s)))
      (nreverse chunks)))

(defun apply-template (template &optional (dictionary {}) (output-stream t))
  (if (stringp template)
      (setf template (read-template template)))
  (dolist (chunk template)
    (typecase chunk
      (string
       (princ chunk output-stream))
      (symbol
       (princ (ref1 dictionary chunk) output-stream))
      (t
       (case (ref1 chunk :type)
         (:format
          (let ((format-string (ref1 chunk :format-string))
                (format-args
                 (loop for arg in (ref1 chunk :format-args)
                    collect (ref1 dictionary arg))))
            (apply #'format output-stream format-string format-args)))
         (:iteration
          (dolist (item (ref1 dictionary (ref1 chunk :sequence)))
            (apply-template (ref1 chunk :body) item output-stream)))
         (t
          (error "don't know that format")))))))

(define-method (trim (s string))
  (flet ((whitespace-char-p (char)
           (or(char= #\space char) (not (graphic-char-p char)))))
    (let ((start (position-if-not #'whitespace-char-p s))
          (end (position-if-not #'whitespace-char-p s :from-end t)))
      (cond ((not start)
             "")
            ((and (= 0 start) (= end (1- (length s))))
             s)
            (t
             (subseq s start (1+ end)))))))

(defun map-lines (fn input-stream)
  (do ((line (read-line input-stream) (read-line input-stream nil 'eof)))
      ((eq line 'eof))
    (funcall fn line)))
