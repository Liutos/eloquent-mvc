(in-package #:eloquent.mvc.contrib)

(defun not-found (request next &key)
  "Send back a 404 response to client if the REQUEST doesn't match any rule. Otherwise, call the NEXT procedures."
  (check-type request eloquent.mvc.request:<request>)
  (check-type next function)
  (if (eloquent.mvc.request:getextra :matched-rule request)
      (funcall next request)
      (eloquent.mvc.response:respond
       ""
       :status 404)))
