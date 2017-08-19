(in-package #:eloquent.mvc.controller)

(defun parse-uri-template (uri-template)
  (declare (type string uri-template))
  (let ((regex (cl-ppcre:regex-replace ":[\\w\\-]+" uri-template "([^/]+)"))
        (vars (mapcar #'(lambda (var)
                          (intern (string-upcase (subseq var 1)) *package*))
                      (cl-ppcre:all-matches-as-strings ":[\\w\\-]+" uri-template))))
    (values vars regex)))

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
