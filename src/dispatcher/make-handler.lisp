(in-package #:eloquent.mvc.dispatcher)

(defun call-action (action request)
  (declare (type symbol action))
  (declare (type eloquent.mvc.request:<request> request))
  (funcall (symbol-function action) request))

(defun make-action-caller (router)
  (lambda (request)
    (let ((action (eloquent.mvc.router:get router request)))
      (call-action action request))))

(defun make-middleware-caller (middlewares action-caller)
  (declare (type (trivial-types:proper-list symbol) middlewares))
  (declare (type function action-caller))
  (reduce #'make-next
          middlewares
          :from-end t
          :initial-value action-caller))

(defun make-next (middleware next)
  (declare (type symbol middleware))
  (lambda (request)
    (declare (type eloquent.mvc.request:<request> request))
    (funcall (symbol-function middleware) request next)))

(defun make-handler (middlewares router)
  (let* ((action-caller (make-action-caller router))
         (middleware-caller (make-middleware-caller middlewares action-caller)))
    (lambda (env)
      (let ((request (eloquent.mvc.request:env-to-request env)))
        (funcall middleware-caller request)))))
