(in-package #:eloquent.mvc.prelude)

(define-condition alist-bind-error ()
  ((message
    :initarg :message
    :reader alist-bind-error-message
    :type string))
  (:report (lambda (c stream)
             (format stream "~A" (alist-bind-error-message c)))))

(defmacro alist-bind (bindings alist-expr &body body)
  "Bind variables to values extracted from ALIST, and evaluate the BODY."
  (alexandria:with-gensyms (alist)
    (labels ((wrap-value (expr &rest args)
               `(compute-value ,expr ,@args))
             (make-binding (binding)
               (destructuring-bind (var field &rest args) binding
                 `(,var ,(apply #'wrap-value
                                `(eloquent.mvc.base:alist-get ,alist ,field)
                                :path field
                                args)))))
      (let ((bindings (mapcar #'make-binding bindings)))
        `(let* ((,alist ,alist-expr)
                ,@bindings)
           ,@body)))))

(defun compute-value (raw
                      &key
                        default
                        path
                        requirep
                        type)
  "Validate and convert RAW to another form according to the keyword arguments."
  (when (and requirep (null raw))
    (error 'alist-bind-error
           :message (format nil "缺少`~A`参数" path)))
  (let ((value raw))
    (when type
      (ecase type
        (:integer (unless (integerp value)
                    (setf value (parse-integer value))))))
    (when (and default (null value))
      (setf value default))
    value))
