(in-package #:eloquent.mvc.logger)

(defclass <log> ()
  ((directory :documentation "Path to the directory for storing log files"
              :initarg :directory
              :reader log-directory
              :type pathname))
  (:documentation "The configuration for generating log files"))
