(in-package #:eloquent.mvc.router)

(defvar *default-action* "ELOQUENT.MVC.ROUTER::NOT-FOUND")

(defun find-rule (router request)
  (with-slots (rules) router
    (find-if #'(lambda (rule)
                 (matchp request rule))
             rules)))

(defun not-found (request)
  (declare (ignore request))
  '(404
    ()
    ()))

(defun get (router request)
  (let ((rule (find-rule router request)))
    (if rule
        (rule-action rule)
        *default-action*)))
