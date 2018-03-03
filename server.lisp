(in-package :fw)

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
          (funcall f env)
        (condition (c)
          `(500
            (:content-type "text/plain")
            ,(list (format nil "~A" c))))))))

(defun start (&key (port 5000))
  (setf *handler*
        (clack:clackup
         (lambda (env)
           (handle env))
         :port port)))

(defun stop ()
  (clack:stop *handler*))
