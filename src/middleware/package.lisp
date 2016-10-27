(defpackage #:eloquent.mvc.middleware
  (:use #:cl)
  (:export #:access-log
           #:compress
           #:parse
           #:parse-body
           #:static-file))
