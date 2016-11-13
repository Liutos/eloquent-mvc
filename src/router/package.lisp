(defpackage #:eloquent.mvc.router
  (:use #:cl)
  (:shadow :get)
  (:export #:get
           #:init
           #:path-info=
           #:rule-action
           #:show
           #:try-request))
