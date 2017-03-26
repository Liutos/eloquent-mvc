(defpackage #:eloquent.mvc.request
  (:use :cl)
  (:export #:<request>
           #:env-to-request
           #:get-cookie
           #:get-header
           #:getextra
           #:request-content-type
           #:request-method
           #:request-path-info
           #:request-query-string
           #:request-remote-addr
           #:request-server-protocol
           #:request-string-body
           #:request-uri
           #:request-url-scheme))
