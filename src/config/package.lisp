(defpackage #:eloquent.mvc.config
  (:use #:cl)
  (:shadow #:get)
  (:export #:*config*
           #:<config>
           #:get
           #:get-application-root
           #:get-cron-jobs
           #:get-cron-log
           #:get-log-directory
           #:get-server-port
           #:get-server-server
           #:get-template-directory
           #:parse))
