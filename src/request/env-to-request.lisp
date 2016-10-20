(in-package #:eloquent.mvc.request)

(defun env-to-request (env)
  (apply #'make-instance '<request> env))
