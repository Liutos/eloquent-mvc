(in-package #:eloquent.mvc.controller)

(defmacro alist-bind (bindings alist-expr &body body)
  "Bind variables to values extracted from ALIST, and evaluate the BODY."
  (alexandria:with-gensyms (alist)
    (labels ((wrap-value (expr &key default type)
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
                                args)))))
      (let ((bindings (mapcar #'make-binding bindings)))
        `(let* ((,alist ,alist-expr)
                ,@bindings)
           ,@body)))))
