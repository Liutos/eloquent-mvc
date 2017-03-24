(in-package #:eloquent.mvc.middleware)

(defun get-content-type (request)
  "Extract the real Content-Type from REQUEST, removing the optional charset part."
  (check-type request eloquent.mvc.request:<request>)
  (let ((content-type (eloquent.mvc.request:request-content-type request)))
    (when content-type
      (apply #'values
             (eloquent.mvc.prelude:split content-type #\;)))))

(defun parse-json-body (string-body)
  (eloquent.mvc.prelude:decode-json-from-string string-body))

(defun parse-form-body (string-body)
  (eloquent.mvc.prelude:parse-query-string string-body))

(defun parse-body (request next &key)
  "Parse the request HTTP body, generate a structured object for further processing, depends on the request CONTENT-TYPE"
  (let ((content-type (get-content-type request)))
    (cond ((string= content-type "application/json")
           (let* ((string-body (eloquent.mvc.request:request-string-body request))
                  (object (parse-json-body string-body)))
             (setf (eloquent.mvc.request:getextra :body request) object)))
          ((string= content-type "application/x-www-form-urlencoded")
           (let* ((string-body (eloquent.mvc.request:request-string-body request))
                  (object (parse-form-body string-body)))
             (setf (eloquent.mvc.request:getextra :body request) object))))
    (funcall next request)))
