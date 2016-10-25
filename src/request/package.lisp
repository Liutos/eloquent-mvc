(defpackage #:eloquent.mvc.request
  (:use :cl)
  (:export #:<request>
           #:env-to-request
           #:get-header
           #:getextra
           #:request-method
           #:request-path-info
           #:request-query-string
           #:request-remote-addr
           #:request-server-protocol
           #:request-uri
           #:request-url-scheme))
