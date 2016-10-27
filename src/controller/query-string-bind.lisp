(in-package #:eloquent.mvc.controller)

(defmacro query-string-bind (bindings request &body body)
  (alexandria:with-gensyms (alist val)
    (let ((bindings (mapcar #'(lambda (binding)
                                (destructuring-bind (var field) binding
                                  `(,var (let ((,val (cdr (assoc ,field ,alist :test #'string=))))
                                           (and ,val (eloquent.mvc.prelude:urldecode ,val))))))
                            bindings)))
      `(let* ((,alist (eloquent.mvc.prelude:parse-query-string
                       (eloquent.mvc.request:request-query-string ,request)))
              ,@bindings)
         ,@body))))
