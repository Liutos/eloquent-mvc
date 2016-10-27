(defpackage #:eloquent.mvc.prelude
  (:use #:cl)
  (:shadow #:dolist)
  (:export #:dolist
           #:equivalent
           #:find-symbol*
           #:make-keyword
           #:parse-query-string
           #:read-file-string
           #:read-lines
           #:split
           #:string-to-alist
           #:urldecode))
