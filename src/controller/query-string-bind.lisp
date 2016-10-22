(in-package #:eloquent.mvc.controller)

(defun parse-query-string (request)
  (let ((query-string (eloquent.mvc.request:request-query-string request)))
    (if (null query-string)
        '()
        (eloquent.mvc.prelude:string-to-alist query-string #\& #\=))))

(defmacro query-string-bind (bindings request &body body)
  (alexandria:with-gensyms (alist)
    (let ((bindings (mapcar #'(lambda (binding)
                                (destructuring-bind (var field) binding
                                  `(,var (cdr (assoc ,field ,alist :test #'string=)))))
                            bindings)))
      `(let* ((,alist (parse-query-string ,request))
              ,@bindings)
         ,@body))))
