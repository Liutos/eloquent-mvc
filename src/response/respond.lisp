(in-package #:eloquent.mvc.response)

(defun respond (body
                &key
                  (headers '())
                  (status 200))
  (make-instance '<response>
                 :body body
                 :status status
                 :headers headers))
