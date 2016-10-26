(in-package #:eloquent.mvc.response)

(defun make-headers (response)
  (with-slots (bytes-sent headers) response
    (override-header headers :content-length bytes-sent)))

(defun override-header (headers field value)
  (declare (type (trivial-types:property-list) headers))
  (declare (type keyword field))
  (let ((filtered (alexandria:remove-from-plist headers field)))
    (cons field (cons value filtered))))

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
