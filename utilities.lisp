;;; Copyright 2008 by Oliver Steele.  Released under the MIT License.

;; this doesn't really test that every element has type TYPE, but
;; I don't think there's a way to do that in CL
(deftype list-type (&optional (type t))
  "(LIST-TYPE {type}) is a LIST whose elements are type {type}."
  `(and list (or null (cons ,type))))

(define-method (trim (s string))
  "Return S without initial or final whitespace characters.
A whitespace character is a space, or a non-graphic character.
If S is all whitespace, the result is the empty string.
Non-consing if the result is STRING= to S."
  (flet ((whitespace-char-p (char)
           (or (char= #\space char) (not (graphic-char-p char)))))
    (let ((start (position-if-not #'whitespace-char-p s))
          (end (position-if-not #'whitespace-char-p s :from-end t)))
      (cond ((not start)
             "")
            ((and (= 0 start) (= end (1- (length s))))
             s)
            (t
             (subseq s start (1+ end)))))))

(defsetf rref setref
    "A setter for dictionaries.")

;; TODO: default slot-names by introspection
(define-method (object->dictionary (object t) reader-names
                                   &key (basename (type-of object)))
  "Create a DICTIONARY whose keys are the names in READER-NAMES, and
whose values are the applications to OBJECT of the functions named by
those objects, prefixed by BASENAME (which defaults the type of OBJECT)."
  (plist->dictionary
   (loop for reader-name in reader-names
      collect reader-name
      collect (funcall (concatenate-symbol basename "-" reader-name)
                       object))))

(defun map-lines (fn input-stream)
  "Apply FN to each line from INPUT-STREAM."
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

(defmacro define-accumulating-method ((function-name (self type))
                                      accumulation-construct
                                      &key child-reader)
  "Defines a method on FUNCTION-NAME, specialized on a first argument of TYPE,
that uses ACCUMALATION-CONSTRUCT (a LOOP-friendly accumulation keyword) to
accumulate the values of the projections of its children.

Children are accessed via CHILD-READER, which defaults to {TYPE}-CHILDREN,
and projected via FUNCTION-NAME."
  (let ((value-reader function-name)
        (child-reader (or child-reader
                          (concatenate-symbol type "-CHILDREN")))
        (child (gensym "child")))
    `(define-method (,function-name (,self ,type))
       (loop for ,child in (,child-reader ,self)
            ,accumulation-construct (,value-reader ,child)))))
