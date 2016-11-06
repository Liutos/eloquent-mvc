(defpackage #:eloquent.mvc.config
  (:use #:cl)
  (:shadow #:get)
  (:export #:<config>
           #:get
           #:get-application-root
           #:get-log-directory
           #:get-server-port
           #:get-server-server
           #:parse))
