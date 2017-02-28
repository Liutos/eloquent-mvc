(in-package #:eloquent.mvc.controller)

(defmacro json-body-bind (bindings request &body body)
  "Bind variables to values in object, which is parsed from JSON HTTP body, by specified keys, and evaluate the body with these bindings"
  (alexandria:with-gensyms (object)
    (labels ((wrap-value (key)
               `(eloquent.mvc.prelude:string-assoc ,key ,object))
             (make-binding (binding)
               (destructuring-bind (var key &rest args) binding
                 `(,var ,(apply #'wrap-value key args)))))
      (let ((bindings (mapcar #'make-binding bindings)))
        `(let* ((,object (eloquent.mvc.request:getextra :body ,request))
                ,@bindings)
           ,@body)))))
