(defpackage #:eloquent.mvc.router
  (:use #:cl)
  (:shadow :get)
  (:export #:get
           #:init
           #:not-found
           #:path-info=
           #:rule-action
           #:show
           #:try-request))
