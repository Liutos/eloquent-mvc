(in-package #:eloquent.mvc.middleware)

(defun parse-json-body (string-body)
  (let ((cl-json:*identifier-name-to-key* #'identity)
        (cl-json:*json-identifier-name-to-lisp* #'identity))
    (cl-json:decode-json-from-string string-body)))

(defun parse-body (request next &key)
  "Parse the request HTTP body, generate a structured object for further processing, depends on the request CONTENT-TYPE"
  (let ((content-type (eloquent.mvc.request:request-content-type request)))
    (cond ((string= content-type "application/json")
           (let* ((string-body (eloquent.mvc.request:request-string-body request))
                  (object (parse-json-body string-body)))
             (setf (eloquent.mvc.request:getextra :body request) object))))
    (funcall next request)))
