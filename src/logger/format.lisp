(in-package #:eloquent.mvc.logger)

(defun make-destination (label log)
  (merge-pathnames (concatenate 'string (string-downcase (symbol-name label)) ".log")
                   (log-directory log)))

(defun format (label control-string &rest format-arguments)
  "Like function CL:FORMAT but output the formatted string into a file named by LABEL."
  (declare (type keyword label))
  (let ((destination (make-destination label *log*)))
    (with-open-file (stream destination
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (apply #'cl:format stream control-string format-arguments))))
