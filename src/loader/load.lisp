(in-package #:eloquent.mvc.loader)

(define-condition project-not-found-error ()
  ((directory
    :documentation "The root directory of a project"
    :initarg :directory))
  (:report (lambda (c stream)
             (format stream "Project in \"~A\" isn't started"
                     (slot-value c 'directory)))))

(define-condition not-directory-error (file-error)
  ()
  (:report (lambda (c stream)
             (format stream "\"~A\" is not an existing directory"
                     (namestring (file-error-pathname c))))))

(defvar *apps*
  (make-hash-table :test #'equal))

(defun make-config-path (directory)
  (merge-pathnames "config/eloquent-mvc.yaml" directory))

(defun make-middleware-path (directory)
  (merge-pathnames "config/middlewares.lisp" directory))

(defun make-router-path (directory)
  (merge-pathnames "config/router.lisp" directory))

;;; EXPORT

(defun load (directory)
  "Load configuration, router and middlewares under DIRECTORY, and start the server for handling client request."
  (check-type directory pathname)
  (unless (uiop:directory-exists-p directory)
    (error 'not-directory-error :pathname directory))

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

(defun reload (directory)
  "Shutdown and restart the application at DIRECTORY."
  (check-type directory pathname)
  (unload directory)
  (load directory))

(defun unload (directory)
  "Unbind the listen on port and stop the server thread."
  (check-type directory pathname)
  (let ((key (namestring directory)))
    (multiple-value-bind (handler found)
        (gethash key *apps*)
      (unless found
        (error 'project-not-found-error :directory directory))
      (stop-server handler)
      (remhash key *apps*))))
