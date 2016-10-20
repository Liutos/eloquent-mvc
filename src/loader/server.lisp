(in-package #:eloquent.mvc.loader)

(defparameter *handler* nil)

(defun start-server (config router)
  (declare (ignore config))
  (setf *handler*
        (clack:clackup (eloquent.mvc.dispatcher:make-handler router))))

(defun stop-server ()
  (clack:stop *handler*)
  (setf *handler* nil))
