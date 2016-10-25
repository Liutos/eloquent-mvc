(in-package #:eloquent.mvc.response)

(defun response-to-list (response)
  (declare (type <response> response))
  (with-slots (body headers status) response
    (let ((body (if (stringp body)
                    (list body)
                    body)))
      `(,status
        ,headers
        ,body))))
