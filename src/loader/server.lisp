(in-package #:eloquent.mvc.loader)

(defparameter *handler* nil)

(defun start-server (config app)
  (let ((port (eloquent.mvc.config:get-server-port config)))
    (setf *handler* (clack:clackup app
                                   :port port))))

(defun stop-server ()
  (clack:stop *handler*)
  (setf *handler* nil))
