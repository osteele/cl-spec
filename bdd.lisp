;;; Author: Yurii Rashkovskii
;;; Source: http://rashkovskii.com/lisp
;;; Fixed compiler errors and warnings --- Oliver Steele 2008-01-05

;; Utilities
(eval-when (:compile-toplevel :load-toplevel)
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
  ())

;; Expectations
(defclass expectation ()
  ((expr :initarg :expr :reader expression-of)
   (args :initarg :args :reader args-of)))

(defclass should (expectation)
  ())

(defgeneric fulfills? (expectation))

(defmethod fulfills? ((e should))
  (flet ((match (matcher-class args expr)
       (restart-case
           (handler-bind ((simple-error #'(lambda (c)
                        (declare (ignore c))
                        (invoke-restart 'fun))))
         (matches? (make-instance matcher-class :args args) expr))
         (fun (&optional v)
           (declare (ignore v))
           (apply matcher-class (append (list (eval expr)) args))))))
    (with-slots (args expr) e
      (if (equal (car args) 'not)
          (not (match (cadr args) (cddr args) expr))
          (match (car args) (cdr args) expr)))))

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
    (let* ((arguments (cdr args))
       (message-forms (mapcar #'(lambda (suffix)
                      (concat-symbol (car args) suffix)) '("" "p" "-p" "?"))))
      (when (equal (car arguments) 'of)
        (pop arguments)) ;; am I crazy?
      (dolist (form message-forms)
        (when (respond-to? expr form arguments)
          (return (eval `(,form ,expr ,@arguments))))))))

(defclass raise (matcher)
  ())

(defmethod matches? ((matcher raise) expr)
  (with-slots (args) matcher
    (restart-case
    (handler-bind ((t #'(lambda (c)
                  (if (equal (class-of c) (find-class (car args)))
                      (invoke-restart 'raises)
                      (invoke-restart 'donot)))))
      (eval `(progn
               (eval ,expr)))
      nil)
      (raises (&optional v)
        (declare (ignore v))
        t)
      (donot (&optional v)
        (declare (ignore v))
        nil))))

;;
(defmacro => (form &rest specification)
  (let ((expectation-class (car specification))
    (args (cdr specification)))
    `(let* ((result ',form)
        (expectation (make-instance ',expectation-class
                      :expr result
                      :args ',args)))
       (unless (fulfills? expectation)
     (error (make-instance 'expectation-not-met)))
       result)))

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
