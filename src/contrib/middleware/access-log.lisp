(in-package #:eloquent.mvc.contrib)

(defun make-time-local (moment)
  (eloquent.mvc.prelude:format-timestring
   nil
   (local-time:universal-to-timestamp moment)
   :nginx-log-format))

(defun access-log (request next &key)
  "For every request, print a NGINX-style log message into file named \"access.log\" under log directory."
  (let* ((moment (get-universal-time))
         (response (funcall next request))
         (bytes-sent (eloquent.mvc.response:response-bytes-sent response))
         (status (eloquent.mvc.response:response-status response)))
    (eloquent.mvc.logger:format
     :access "~A - - [~A] \"~A ~A ~A\" ~D ~D \"-\" \"~A\"~%"
     (eloquent.mvc.request:request-remote-addr request)
     (make-time-local moment)
     (eloquent.mvc.request:request-method request)
     (eloquent.mvc.request:request-uri request)
     (eloquent.mvc.request:request-server-protocol request)
     status
     bytes-sent
     (eloquent.mvc.request:get-header request "user-agent"))
    response))
