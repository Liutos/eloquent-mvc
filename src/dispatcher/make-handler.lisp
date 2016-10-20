(in-package #:eloquent.mvc.dispatcher)

(defun call-action (action env)
  (declare (type string action))
  (destructuring-bind (package-name symbol-name)
      (eloquent.mvc.prelude:split (string-upcase action) #\:)
    (let ((package (find-package package-name)))
      (uiop:symbol-call package symbol-name env))))

(defun make-handler (router)
  (lambda (env)
    (let* ((request (eloquent.mvc.request:env-to-request env))
           (action (eloquent.mvc.router:get router request)))
      (call-action action env))))
