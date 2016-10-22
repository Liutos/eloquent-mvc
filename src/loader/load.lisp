(in-package #:eloquent.mvc.loader)

(defun make-config-path (directory)
  (merge-pathnames "config/eloquent-mvc.ini" directory))

(defun make-middleware-path (directory)
  (merge-pathnames "config/middlewares" directory))

(defun make-router-path (directory)
  (merge-pathnames "config/router" directory))

(defun load (directory)
  (let ((config-path (make-config-path directory))
        (middlewares-path (make-middleware-path directory))
        (router-path (make-router-path directory)))
    (let ((config (eloquent.mvc.config:parse config-path))
          (middlewares (eloquent.mvc.middleware:parse middlewares-path))
          (router (eloquent.mvc.router:parse router-path)))
      (start-server config (eloquent.mvc.dispatcher:make-handler middlewares router)))))
