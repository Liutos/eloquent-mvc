(in-package #:eloquent.mvc.middleware)

(defun set-matched-rule (request next &key)
  (format t "you should see me~%")
  (let ((method (eloquent.mvc.request:request-method request))
        (path (eloquent.mvc.request:request-path-info request)))
    (let ((rule (eloquent.mvc.router:try-request method path)))
      (setf (eloquent.mvc.request:getextra :matched-rule request) rule)
      (funcall next request))))
