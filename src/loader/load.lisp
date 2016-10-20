(in-package #:eloquent.mvc.loader)

(defun make-config-path (directory)
  (merge-pathnames "config/eloquent-mvc.ini" directory))

(defun make-router-path (directory)
  (merge-pathnames "config/router" directory))

(defun load (directory)
  (let ((config-path (make-config-path directory))
        (router-path (make-router-path directory)))
    (let ((config (eloquent.mvc.config:parse config-path))
          (router (eloquent.mvc.router:parse router-path)))
      (start-server config router))))
