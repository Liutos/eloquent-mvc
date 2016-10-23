(in-package #:eloquent.mvc.router)

(defun find-rule (router request)
  (with-slots (rules) router
    (find-if #'(lambda (rule)
                 (matchp request rule))
             rules)))

(defun not-found (request)
  (declare (ignore request))
  (eloquent.mvc.response:respond
   ""
   :status 404))

(defun get (router request)
  (let ((rule (find-rule router request)))
    (if rule
        (rule-action rule)
        'not-found)))
