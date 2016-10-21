(in-package #:eloquent.mvc.dispatcher)

(defun call-action (action env)
  (funcall (symbol-function action) env))

(defun make-handler (router)
  (lambda (env)
    (let* ((request (eloquent.mvc.request:env-to-request env))
           (action (eloquent.mvc.router:get router request)))
      (call-action action env))))
