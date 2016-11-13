(in-package #:eloquent.mvc.middleware)

(defun set-matched-rule (request next &key)
  "Find the matching routing rule from router table, set to :matched-url key under the extras of REQUEST, and call the next procedures."
  (check-type request eloquent.mvc.request:<request>)
  (check-type next function)
  (let ((method (eloquent.mvc.request:request-method request))
        (path (eloquent.mvc.request:request-path-info request)))
    (let ((rule (eloquent.mvc.router:try-request method path)))
      (setf (eloquent.mvc.request:getextra :matched-rule request) rule)
      (funcall next request))))
