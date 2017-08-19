(defpackage #:eloquent.mvc.router
  (:use #:cl)
  (:shadow :get)
  (:export #:get
           #:init
           #:maprouter
           #:path-info=
           #:print-rule
           #:rule-action
           #:show
           #:try-request))
