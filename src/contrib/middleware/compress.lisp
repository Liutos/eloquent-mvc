(in-package #:eloquent.mvc.contrib)

(defun compress (request next &key)
  "Compress the response body into gzip format when available"
  (let ((accept-encoding (eloquent.mvc.request:get-header request "accept-encoding"))
        (response (funcall next request)))
    (when (search "gzip" accept-encoding)
      (with-accessors ((body eloquent.mvc.response:response-body)
                       (headers eloquent.mvc.response:response-headers))
          response
        (when (stringp body)
          (let ((bytes (salza2:compress-data (flexi-streams:string-to-octets body :external-format :utf-8)
                                             'salza2:gzip-compressor))
                (bytes-sent (eloquent.mvc.response:response-bytes-sent response)))
            (when (< (length bytes) bytes-sent)
              (setf body bytes
                    headers (eloquent.mvc.response:override-header
                             headers :content-encoding "gzip")))))))
    response))
