(defpackage #:eloquent.mvc.config
  (:use #:cl)
  (:shadow #:get)
  (:export #:get
           #:get-application-root
           #:get-server-port
           #:parse))
