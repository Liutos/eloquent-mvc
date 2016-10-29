(in-package #:eloquent.mvc.controller)

(defmacro query-string-bind (bindings request &body body)
  (alexandria:with-gensyms (alist)
    (let ((bindings (mapcar #'(lambda (binding)
                                (destructuring-bind (var field) binding
                                  `(,var (eloquent.mvc.prelude:string-assoc ,field ,alist))))
                            bindings)))
      `(let* ((,alist (eloquent.mvc.prelude:parse-query-string
                       (eloquent.mvc.request:request-query-string ,request)))
              ,@bindings)
         ,@body))))
