(in-package :fw)

(defparameter *middlewares*
  (list
   'response
   'visitor))

(defvar *handler*)

(defun handle (env)
  (let ((path (path-of env))
        (method (method-of env)))
    (let (f
          (route (find-route method path)))
      (cond (route
             (setf f (symbol-function (third route)))
             (setf (getf env :params) (extract-from-path env route)))
            (t
             (setf f #'route-not-found)))
      (handler-case
          (let* ((action-caller (lambda (env)
                                  (funcall f env)))
                 (middleware-caller (make-middleware-caller *middlewares* action-caller)))
            (funcall middleware-caller env))
        (condition (c)
          `(500
            (:content-type "text/plain")
            ,(list (format nil "~A" c))))))))

(defun make-middleware-caller (middlewares action-caller)
  (flet ((make-next (middleware next)
           (declare (type symbol middleware))
           (lambda (env)
             (funcall (symbol-function middleware) env next))))
    (reduce #'make-next
            middlewares
            :from-end t
            :initial-value action-caller)))


(defun start (&key (port 5000))
  (setf *handler*
        (clack:clackup
         (lambda (env)
           (handle env))
         :port port)))

(defun stop ()
  (clack:stop *handler*))
