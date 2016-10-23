(in-package #:eloquent.mvc.response)

(defun response-to-list (response)
  (declare (type <response> response))
  (with-slots (body headers status) response
    `(,status
      ,headers
      (,body))))
