(in-package #:eloquent.mvc.controller)

(defmacro form-bind (bindings request &body body)
  "Bind variables with values extracted from REQUEST's HTTP body which is encoded like a HTML form, and evaluate BODY with these BINDINGS."
  `(alist-bind
       ,bindings
       (eloquent.mvc.request:getextra :body ,request)
     ,@body))
