(in-package #:eloquent.mvc.dispatcher)

(defun call-action (action request)
  (declare (type symbol action))
  (declare (type eloquent.mvc.request:<request> request))
  (funcall (symbol-function action) request))

(defun make-action-caller ()
  (lambda (request)
    (let ((action (eloquent.mvc.router:get request)))
      (call-action action request))))

(defun make-middleware-caller (config middlewares action-caller)
  (declare (type (trivial-types:proper-list symbol) middlewares))
  (declare (type function action-caller))
  (flet ((make-next (middleware next)
           (declare (type symbol middleware))
           (lambda (request)
             (declare (type eloquent.mvc.request:<request> request))
             (funcall (symbol-function middleware) request next
                      :config config
                      :allow-other-keys t))))
    (reduce #'make-next
            middlewares
            :from-end t
            :initial-value action-caller)))

;;; EXPORT

(defun make-handler (config middlewares)
  (let* ((action-caller (make-action-caller))
         (middleware-caller (make-middleware-caller config middlewares action-caller)))
    (lambda (env)
      (let ((request (eloquent.mvc.request:env-to-request env)))
        (eloquent.mvc.response:response-to-list
         (funcall middleware-caller request))))))
