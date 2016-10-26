(in-package #:eloquent.mvc.logger)

(defparameter *log* nil)

(defun init (config)
  (declare (type eloquent.mvc.config:<config> config))
  (let* ((directory (eloquent.mvc.config:get-log-directory config))
         (log (make-instance '<log>
                             :directory directory)))
    (setf *log* log)
    log))
