;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.

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

;; TODO: default slot-names by introspection
(define-method (object->dictionary (object t) reader-names &key basename)
  (plist->dictionary
   (loop for reader-name in reader-names
        collect reader-name
        collect (funcall (concatenate-symbol basename "-" reader-name)
                         object))))

(defun map-lines (fn input-stream)
  (do ((line (read-line input-stream) (read-line input-stream nil 'eof)))
      ((eq line 'eof))
    (funcall fn line)))

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
