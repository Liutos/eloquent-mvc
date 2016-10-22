(in-package #:eloquent.mvc.middleware)

(defun parse (file)
  (let ((lines (eloquent.mvc.prelude:read-lines file)))
    (mapcar #'eloquent.mvc.prelude:find-symbol* lines)))
