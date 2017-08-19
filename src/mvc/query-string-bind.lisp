(in-package #:eloquent.mvc.controller)

(defmacro query-string-bind (bindings request &body body)
  "Bind values extract from query string of REQUEST to variables declared in BINDINGS, and execute BODY in this extended context."
  `(alist-bind
       ,bindings
       (eloquent.mvc.prelude:parse-query-string
        (eloquent.mvc.request:request-query-string ,request))
     ,@body))
