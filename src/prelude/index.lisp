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
    (labels ((wrap-value (expr &key default path requirep type)
               (when requirep
                 (setf expr `(or ,expr
                                 (error 'alist-bind-error
                                        :message (format nil "缺少`~A`参数" ,path)))))
               (when type
                 (ecase type
                   (:integer (setf expr (alexandria:with-gensyms (val)
                                          `(let ((,val ,expr))
                                             (and ,val (parse-integer ,val))))))))
               (when default
                 (setf expr `(or ,expr ,default)))
               expr)
             (make-binding (binding)
               (destructuring-bind (var field &rest args) binding
                 `(,var ,(apply #'wrap-value
                                `(eloquent.mvc.prelude:string-assoc ,field ,alist)
                                :path field
                                args)))))
      (let ((bindings (mapcar #'make-binding bindings)))
        `(let* ((,alist ,alist-expr)
                ,@bindings)
           ,@body)))))
