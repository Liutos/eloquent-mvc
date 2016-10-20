(in-package #:eloquent.mvc.loader)

(defparameter *handler* nil)

(defun make-config-path (directory)
  (merge-pathnames "config/eloquent-mvc.ini" directory))

(defun make-router-path (directory)
  (merge-pathnames "config/router" directory))

(defun start-server (config router)
  (declare (ignore config))
  (setf *handler*
        (clack:clackup (eloquent.mvc.dispatcher:make-handler router))))

(defun load (directory)
  (let ((config-path (make-config-path directory))
        (router-path (make-router-path directory)))
    (let ((config (eloquent.mvc.config:parse config-path))
          (router (eloquent.mvc.router:parse router-path)))
      (start-server config router))))
