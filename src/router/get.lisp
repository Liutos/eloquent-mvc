(in-package #:eloquent.mvc.router)

(defun find-rule (router request)
  (with-slots (rules) router
    (find-if #'(lambda (rule)
                 (matchp request rule))
             rules)))

(defun get (router request)
  (let ((rule (find-rule router request)))
    (rule-action rule)))
