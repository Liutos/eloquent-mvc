(in-package :fw)

(defvar *handler*)

(defun handle (env)
  (let ((path (path-of env))
        (method (method-of env)))
    (let ((route (find-route method path)))
      (let ((f (if route (symbol-function (third route)) #'route-not-found)))
        (handler-case
            (funcall f env)
          (condition (c)
            `(500
              (:content-type "text/plain")
              ,(list (format nil "~A" c)))))))))

(defun start ()
  (setf *handler*
        (clack:clackup
         (lambda (env)
           (handle env)))))

(defun stop ()
  (clack:stop *handler*))
