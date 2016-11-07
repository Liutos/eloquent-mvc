(in-package #:eloquent.mvc.request)

(defun get-header (request field)
  "Return the value of FIELD in request's headers."
  (declare (type string field))
  (with-slots (headers) request
    (gethash field headers)))
