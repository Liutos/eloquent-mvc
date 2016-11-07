(in-package #:eloquent.mvc.middleware)

(defun parse-middleware (name)
  (multiple-value-bind (symbol status)
      (eloquent.mvc.prelude:find-symbol* name)
    (assert (eq status :external))
    symbol))

(defun parse (file)
  "Read the middlewares specified in FILE."
  (let ((lines (eloquent.mvc.prelude:read-lines file)))
    (mapcar #'parse-middleware lines)))
