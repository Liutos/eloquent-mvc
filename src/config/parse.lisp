(in-package #:eloquent.mvc.config)

(defun parse (filename)
  (declare (type pathname filename))
  (let ((content (cl-yaml:parse filename)))
    (make-instance '<config>
                   :content content)))
