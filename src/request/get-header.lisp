(in-package #:eloquent.mvc.request)

(defun get-header (request field)
  (declare (type string field))
  (with-slots (headers) request
    (gethash field headers)))
