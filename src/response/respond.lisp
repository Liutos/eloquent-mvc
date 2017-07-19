(in-package #:eloquent.mvc.response)

(defun respond (body
                &key
                  (encoder #'identity)
                  (headers '())
                  (status 200))
  (when (keywordp encoder)
    (setf encoder (make-encoder encoder)))
  (make-instance '<response>
                 :body (funcall encoder body)
                 :status status
                 :headers headers))

(defun respond-json (body
                     &key
                       (headers '())
                       (status 200))
  "Encoding body into JSON and send response with at least the header Content-Type equals \"application/json\""
  (setf headers
        (override-header headers :content-type "application/json"))
  (respond
   (cl-json:encode-json-to-string body)
   :headers headers
   :status status))
