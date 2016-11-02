(in-package #:eloquent.mvc.loader)

(defvar *apps*
  (make-hash-table :test #'equal))

(defun make-config-path (directory)
  (merge-pathnames "config/eloquent-mvc.yaml" directory))

(defun make-middleware-path (directory)
  (merge-pathnames "config/middlewares" directory))

(defun make-router-path (directory)
  (merge-pathnames "config/router.lisp" directory))

;;; EXPORT

(defun load (directory)
  (let ((config-path (make-config-path directory))
        (middlewares-path (make-middleware-path directory))
        (router-path (make-router-path directory)))
    (let ((config (eloquent.mvc.config:parse config-path))
          (key (namestring directory))
          (middlewares (eloquent.mvc.middleware:parse middlewares-path)))
      (eloquent.mvc.logger:init config)
      (eloquent.mvc.router:init router-path)
      (setf (gethash key *apps*)
            (start-server config (eloquent.mvc.dispatcher:make-handler config middlewares))))))

(defun unload (directory)
  (let ((key (namestring directory)))
    (stop-server (gethash key *apps*))
    (remhash key *apps*)))
