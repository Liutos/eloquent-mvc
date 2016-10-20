(defpackage #:eloquent.mvc.request
  (:use :cl)
  (:export #:<request>
           #:env-to-request
           #:request-method
           #:request-path-info))
