(in-package #:eloquent.mvc.logger)

(defparameter *log* nil
  "The object contains configuration for logging system.")

(defun init (config)
  "Initialize the state of logging system."
  (declare (type eloquent.mvc.config:<config> config))
  (let* ((directory (eloquent.mvc.config:get-log-directory config))
         (log (make-instance '<log>
                             :directory directory)))
    (setf *log* log)
    log))
