(in-package #:eloquent.mvc.contrib)

(defun fill-template (request next &key config)
  "If specified, fill a template by the return value of applying NEXT on REQUEST, and send the string generated by template back to client."
  (check-type request eloquent.mvc.request:<request>)
  (check-type next function)
  (let ((response (funcall next request)))
    (when (typep response 'eloquent.mvc.response:<response>)
      (return-from fill-template response))
    (let* ((rule (eloquent.mvc.request:getextra :matched-rule request))
           (action (eloquent.mvc.router:rule-action rule))
           (content-type (get action :content-type "text/html"))
           (template (pathname (get action :template))))
      (let ((html-template:*default-template-pathname*
             (eloquent.mvc.config:get-template-directory config)))
        (let ((text (with-output-to-string (stream)
                      (html-template:fill-and-print-template template response
                                                             :stream stream))))
          (eloquent.mvc.response:respond
           text
           :headers (list :content-type content-type)))))))
