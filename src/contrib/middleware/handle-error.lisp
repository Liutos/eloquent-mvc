(in-package #:eloquent.mvc.contrib)

(defun error-log (e)
  "Write the message and backtrace of E to error log file."
  (let ((body (error-to-body e)))
    (dolist (line (eloquent.mvc.prelude:split body #\Newline
                                              :remove-empty-subseqs t))
      (eloquent.mvc.logger:format
       :error "~A~%" line))))

(defun error-to-body (e)
  "Convert the backtrace of E into a string."
  (check-type e error)
  (trivial-backtrace:print-backtrace e :output nil))

;;; EXPORT

(defun handle-error (request next &key)
  "Execute the NEXT, catch the error it signaled, if any, and make a HTTP error response."
  (check-type request eloquent.mvc.request:<request>)
  (check-type next function)
  (handler-case (funcall next request)
    (eloquent.mvc.response:http-compatible-error (e)
      (eloquent.mvc.response:respond
       (eloquent.mvc.response:http-message e)
       :status (eloquent.mvc.response:http-status e)))
    (error (e)
      (error-log e)
      (let ((body (error-to-body e)))
        (eloquent.mvc.response:respond
         body
         :headers '(:content-type "text/plain")
         :status 500)))))
