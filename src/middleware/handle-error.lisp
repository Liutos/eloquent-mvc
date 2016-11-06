(in-package #:eloquent.mvc.middleware)

(defun handle-error (request next &key)
  "Execute the NEXT, catch the error it signaled, if any, and make a HTTP error response."
  (check-type request eloquent.mvc.request:<request>)
  (check-type next function)
  (handler-case (funcall next request)
    (error ()
      (eloquent.mvc.response:respond
       ""
       :status 500))))
