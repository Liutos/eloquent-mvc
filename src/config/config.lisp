(in-package #:eloquent.mvc.config)

(defclass <config> ()
  ((content :documentation "Object generated by CL-YAML:PARSE"
            :initarg :content
            :reader config-content
            :type hash-table))
  (:documentation "Configurations for the eloquent-mvc project"))

(defun get (config section-name option-name)
  (declare (type string option-name section-name))
  (declare (type <config> config))
  (with-slots (content) config
    (multiple-value-bind (section found)
        (gethash section-name content)
      (when found
        (gethash option-name section)))))

(defun get-application-root (config)
  (get config "application" "root"))

(defun get-server-port (config)
  (get config "server" "port"))
