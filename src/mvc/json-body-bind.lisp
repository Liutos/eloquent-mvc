(in-package #:eloquent.mvc.controller)

(defmacro json-body-bind (bindings request &body body)
  "Bind variables to values in object, which is parsed from JSON HTTP body, by specified keys, and evaluate the body with these bindings"
  `(alist-bind
       ,bindings
       (eloquent.mvc.request:getextra :body ,request)
     ,@body))
