(in-package #:eloquent.mvc.middleware)

(defun error-to-body (e)
  "Convert the backtrace of E into a string."
  (check-type e error)
  (trivial-backtrace:print-backtrace e :output nil))

(defun handle-error (request next &key)
  "Execute the NEXT, catch the error it signaled, if any, and make a HTTP error response."
  (check-type request eloquent.mvc.request:<request>)
  (check-type next function)
  (handler-case (funcall next request)
    (error (e)
      (let ((body (error-to-body e)))
        (eloquent.mvc.response:respond
         body
         :headers '(:content-type "text/plain")
         :status 500)))))
