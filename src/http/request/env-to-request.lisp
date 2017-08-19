(in-package #:eloquent.mvc.request)

(defun env-to-request (env)
  "Initialize a instance of ``eloquent.mvc.request:<request>'' by key-values from ENV."
  (apply #'make-instance '<request>
         :allow-other-keys t
         env))
