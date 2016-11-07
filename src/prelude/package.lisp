(defpackage #:eloquent.mvc.prelude
  (:use #:cl)
  (:shadow #:dolist)
  (:export #:dolist
           #:find-symbol*
           #:make-keyword
           #:parse-query-string
           #:read-file-string
           #:read-lines
           #:split
           #:string-assoc
           #:string-to-alist
           #:urldecode))
