;;; Author: Yurii Rashkovskii
;;; Source: http://rashkovskii.com/lisp

;;; ows 2008-01-05 -- fixed compiler errors and warnings
;;; ows 2008-01-08 -- eval -> funcall, to capture lexical scope

;; Utilities
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod obj->string ((s string))
    s)

  (defmethod obj->string ((s symbol))
    (string s))

  (defun concat-symbol (&rest args)
    (intern (apply #'concatenate 'string
                   (mapcar #'string-upcase (mapcar #'obj->string args)))))
  )

(defun respond-to? (o method &rest args)
  (restart-case
      (handler-bind ((undefined-function #'(lambda (c)
                         (declare (ignore c))
                         (invoke-restart 'no)))
             (simple-error #'(lambda (c)
                       (declare (ignore c))
                       (invoke-restart 'no))))
    (let ((sf (symbol-function method))
          (cpl (sb-pcl::class-precedence-list (class-of o))))
      (typecase sf
        (standard-generic-function
         (find t
           (mapcar #'(lambda (klass)
                   (not (null
                     (find-method sf '()
                          (cons klass
                            (mapcar #'(lambda (c) (declare (ignore c)) t) args)) nil))))
               cpl)))
        (function t))))
    (no (&optional v)
      (declare (ignore v))
      nil)))

;; Conditions

(define-condition expectation-not-met ()
  ((expectation :initarg :expectation :reader expectation)
   (form :initarg :form :reader expectation-form)))

;; Expectations
(defclass expectation ()
  ((value-fn :initarg :value-fn :reader value-fn-of)
   (value :accessor value-of)
   (args :initarg :args :reader args-of)))

(defclass should (expectation)
  ())

(defmethod evaluate ((e expectation))
  (setf (value-of e) (funcall (value-fn-of e))))

(defgeneric fulfills? (expectation))

(defmethod fulfills? ((e should))
  (flet ((match (matcher-class args)
           (restart-case
           (handler-bind ((simple-error #'(lambda (c)
                                            (declare (ignore c))
                                            (invoke-restart 'fun))))
             (matches? (make-instance matcher-class :args args) e))
           (fun (&optional v)
             ;; This happens when matcher-class doesn't actually name
             ;; a class; e.g. (=> 1 should = 1) instead of
             ;; (=> 0 should be zero)
             (declare (ignore v))
             (apply matcher-class (append (list (evaluate e)) (mapcar #'eval args)))))))
    (with-slots (args) e
      (if (eq (car args) 'not)
          (not (match (cadr args) (cddr args)))
          (match (car args) (cdr args))))))

;; Matchers

(defclass matcher ()
  ((args :initarg :args :reader args-of)))

(defclass be (matcher)
  ())

(defmethod initialize-instance :after ((matcher be) &rest initargs)
  (declare (ignore initargs))
  (with-slots (args) matcher
    (when (equal (car args) 'a)
      (pop args))))

(defgeneric matches? (matcher expr))

(defmethod matches? ((matcher be) expr)
  (with-slots (args) matcher
    (let* ((value (evaluate expr))
           (arguments (cdr args))
           (message-forms (mapcar #'(lambda (suffix)
                                      (concat-symbol (car args) suffix))
                                  '("" "p" "-p" "?"))))
      (when (equal (car arguments) 'of)
        (pop arguments)) ;; am I crazy?
      (setf arguments (mapcar #'eval arguments))
      (some #'(lambda (form)
                (and (respond-to? value form arguments)
                     (apply form value arguments)))
            message-forms))))

(defclass raise (matcher)
  ())

(defmethod matches? ((matcher raise) e)
  (with-slots (args) matcher
    (let ((class-name (car args)))
      (restart-case
          (handler-bind ((t #'(lambda (c)
                                (setf (value-of e) c)
                                (if (typep c class-name)
                                    (invoke-restart 'raises)
                                    (invoke-restart 'donot)))))
            (evaluate e)
            nil)
        (raises (&optional v)
          (declare (ignore v))
          t)
        (donot (&optional v)
          (declare (ignore v))
          nil)))))

;;
(defmacro => (form &rest specification)
  (let ((expectation-class (car specification))
    (args (cdr specification)))
    `(let ((expectation
            (make-instance ',expectation-class
                           :value-fn #'(lambda () ,form)
                           :args ',args)))
       (unless (fulfills? expectation)
         (error (make-instance 'expectation-not-met
         :expectation expectation
         :form ',form)))
       (value-of expectation))))

;; Grouping
(defmacro define-with-spec-grouping (name)
  (let ((with-grouping (concat-symbol "with-" name ))
    (spec-groupings (concat-symbol "*spec-" name "s*"))
    (spec-grouping (concat-symbol "*spec-" name "*")))
    `(defmacro ,with-grouping (grouping-name &body body)
       `(progn
      (unless (and (boundp ',',spec-groupings) (listp ,',spec-groupings))
        (defvar ,',spec-groupings nil))
      (let* ((,',spec-groupings (cons ,grouping-name ,',spec-groupings))
         (,',spec-grouping (car ,',spec-groupings)))
        ,@body)))))

(define-with-spec-grouping context)
(define-with-spec-grouping aspect)

(defmacro specify (name &body body)
  `(let ((*spec-specification* ,name))
     ,@body))

(provide 'bdd)
