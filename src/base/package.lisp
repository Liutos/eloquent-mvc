(defpackage #:eloquent.mvc.prelude
  (:nicknames #:eloquent.mvc.base)
  (:use #:cl)
  (:export #:alist-bind
           #:alist-bind-error
           #:alist-bind-error-message
           ;; json
           #:alist-get
           #:decode-json-to-alist
           #:encode-alist-to-json

           #:decode-json-from-string
           #:format-timestring
           #:find-symbol*
           #:make-keyword
           #:now
           #:parse-cookie-string
           #:parse-query-string
           #:read-file-string
           #:read-lines
           #:split
           #:string-assoc
           #:string-to-alist
           #:urldecode))
