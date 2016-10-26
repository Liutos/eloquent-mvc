(defpackage #:eloquent.mvc.router
  (:use #:cl)
  (:shadow :get)
  (:export #:get
           #:not-found
           #:parse))
