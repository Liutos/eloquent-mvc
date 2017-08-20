(defpackage #:eloquent.mvc.controller
  (:use #:cl)
  (:export #:form-bind
           #:json-body-bind
           #:query-string-bind
           #:url-bind))

(in-package #:eloquent.mvc.controller)

(defmacro alist-bind (bindings alist-expr &body body)
  `(handler-case
       (eloquent.mvc.prelude:alist-bind ,bindings ,alist-expr ,@body)
     (eloquent.mvc.prelude:alist-bind-error (e)
       (error 'eloquent.mvc.response:http-compatible-error
              :message (eloquent.mvc.prelude:alist-bind-error-message e)
              :status 400))))

(defun parse-uri-template (uri-template)
  (declare (type string uri-template))
  (let ((regex (cl-ppcre:regex-replace ":[\\w\\-]+" uri-template "([^/]+)"))
        (vars (mapcar #'(lambda (var)
                          (intern (string-upcase (subseq var 1)) *package*))
                      (cl-ppcre:all-matches-as-strings ":[\\w\\-]+" uri-template))))
    (values vars regex)))

;;; export

(defmacro form-bind (bindings request &body body)
  "Bind variables with values extracted from REQUEST's HTTP body which is encoded like a HTML form, and evaluate BODY with these BINDINGS."
  `(alist-bind
       ,bindings
       (eloquent.mvc.request:getextra :body ,request)
     ,@body))

(defmacro json-body-bind (bindings request &body body)
  "Bind variables to values in object, which is parsed from JSON HTTP body, by specified keys, and evaluate the body with these bindings"
  `(alist-bind
       ,bindings
       (eloquent.mvc.request:getextra :body ,request)
     ,@body))

(defmacro query-string-bind (bindings request &body body)
  "Bind values extract from query string of REQUEST to variables declared in BINDINGS, and execute BODY in this extended context."
  `(alist-bind
       ,bindings
       (eloquent.mvc.prelude:parse-query-string
        (eloquent.mvc.request:request-query-string ,request))
     ,@body))

(defmacro url-bind (uri-template request &body body)
  "Bind variables to components in request URL

The components start with a colon and followed a series of characters satisfy ALPHANUMERICP, will be converted to variable names in bindings. And each variables' value is the sub-string in the URL, occupying the same place. For example, if the `uri-template` is \"/post/:id\" and path of request is \"/post/123\", then the lexical variable named ID will be declared, and bind to string \"abc\""
  (multiple-value-bind (vars regex)
      (parse-uri-template uri-template)
    (alexandria:with-gensyms (path-info result-form)
      `(let ((,path-info (eloquent.mvc.request:request-path-info ,request))
             ,result-form)
         (cl-ppcre:do-register-groups ,vars
             (,regex ,path-info ,result-form)
           (setf ,result-form
                 (progn ,@body)))))))
