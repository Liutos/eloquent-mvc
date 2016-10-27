(in-package #:eloquent.mvc.controller)

(defmacro json-body-bind (bindings request &body body)
  "Bind variables to values in object, which is parsed from JSON HTTP body, by specified keys, and evaluate the body with these bindings"
  (alexandria:with-gensyms (object)
    (let ((bindings (mapcar #'(lambda (binding)
                                (destructuring-bind (var key) binding
                                  `(,var (cdr (assoc ,key ,object :test #'string=)))))
                            bindings)))
      `(let* ((,object (eloquent.mvc.request:getextra :body ,request))
              ,@bindings)
         ,@body))))
