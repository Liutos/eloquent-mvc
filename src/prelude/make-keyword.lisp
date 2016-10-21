(in-package #:eloquent.mvc.prelude)

(defun make-keyword (s)
  (intern (string-upcase s) :keyword))
