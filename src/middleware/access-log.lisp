(in-package #:eloquent.mvc.middleware)

(defun make-time-local (moment)
  (local-time:format-timestring
   nil
   (local-time:universal-to-timestamp moment)
   :format '((:day 2) "/" :short-month "/" (:year 2) ":" (:hour 2) ":" (:min 2) ":" (:sec 2) " " :gmt-offset-hhmm)))

(defun access-log (request next &key)
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
