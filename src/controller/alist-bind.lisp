(in-package #:eloquent.mvc.controller)

(defmacro alist-bind (bindings alist-expr &body body)
  `(handler-case
       (eloquent.mvc.prelude:alist-bind ,bindings ,alist-expr ,@body)
     (eloquent.mvc.prelude:alist-bind-error (e)
       (error 'eloquent.mvc.response:http-compatible-error
              :message (eloquent.mvc.prelude:alist-bind-error-message e)
              :status 400))))
