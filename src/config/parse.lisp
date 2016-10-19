(in-package #:eloquent.mvc.config)

(defun parse (filename)
  (let ((content (py-configparser:make-config)))
    (py-configparser:read-files content (list filename))
    (make-instance '<config>
                   :content content)))
