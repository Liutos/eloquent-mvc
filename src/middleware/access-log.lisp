(in-package #:eloquent.mvc.middleware)

(defun make-time-local (moment)
  (local-time:format-timestring
   nil
   (local-time:universal-to-timestamp moment)
   :format '(:day "/" :short-month "/" :year ":" :hour ":" :min ":" :sec " " :gmt-offset-hhmm)))

(defun access-log (request next)
  (let* ((moment (get-universal-time))
         (response (funcall next request))
         (body (first (third response)))
         (bytes-sent (length body))
         (status (first response)))
    (format t "~A - - [~A] \"~A ~A ~A\" ~D ~D \"-\" \"~A\""
            (eloquent.mvc.request:request-remote-addr request)
            (make-time-local moment)
            (eloquent.mvc.request:request-method request)
            (eloquent.mvc.request:request-uri request)
            (eloquent.mvc.request:request-server-protocol request)
            status
            bytes-sent
            (eloquent.mvc.request:get-header request "user-agent"))
    response))
