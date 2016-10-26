(defpackage #:eloquent.mvc.prelude
  (:use #:cl)
  (:shadow #:dolist)
  (:export #:dolist
           #:equivalent
           #:find-symbol*
           #:make-keyword
           #:read-file-string
           #:read-lines
           #:split
           #:string-to-alist
           #:urldecode))
