(in-package #:eloquent.mvc.prelude)

(defmacro dolist ((lambda-list list &optional result) &body body)
  (alexandria:with-gensyms (var)
    (let ((lambda-list (alexandria:ensure-list lambda-list)))
      `(cl:dolist (,var ,list ,result)
         (destructuring-bind ,lambda-list ,var
           ,@body)))))
