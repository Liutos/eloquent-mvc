(in-package #:eloquent.mvc.dispatcher)

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
  "Return a newly created function for handling client request passed from function CLACK:CLACKUP."
  (pushnew 'eloquent.mvc.middleware:set-matched-rule middlewares)
  (let* ((action-caller (lambda (request)
                          (eloquent.mvc.middleware:apply-matched-rule request (values))))
         (middleware-caller (make-middleware-caller config middlewares action-caller)))
    (lambda (env)
      (let ((eloquent.mvc.config:*config* config)
            (eloquent.mvc.middleware:*middlewares* middlewares))
        (let ((request (eloquent.mvc.request:env-to-request env)))
          (eloquent.mvc.response:response-to-list
           (funcall middleware-caller request)))))))
