(defpackage #:eloquent.mvc.logger
  (:use #:cl)
  (:shadow #:format)
  (:export #:*log*
           #:format
           #:init))
