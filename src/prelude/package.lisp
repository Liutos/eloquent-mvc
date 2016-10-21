(defpackage #:eloquent.mvc.prelude
  (:use #:cl)
  (:shadow #:dolist)
  (:export #:dolist
           #:equivalent
           #:make-keyword
           #:read-file-string
           #:split))
