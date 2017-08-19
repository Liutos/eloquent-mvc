(in-package #:eloquent.mvc.response)

(defun make-headers (response)
  (with-slots (bytes-sent headers) response
    (override-header headers :content-length bytes-sent)))

(defun response-to-list (response)
  (declare (type <response> response))
  (with-slots (body headers status) response
    (let ((body (if (stringp body)
                    (list body)
                    body))
          (headers (make-headers response)))
      `(,status
        ,headers
        ,body))))
