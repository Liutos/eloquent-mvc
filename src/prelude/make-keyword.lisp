(in-package #:eloquent.mvc.prelude)

(defun make-keyword (s)
  "Intern the string S into package :KEYWORD, upcase it at first."
  (intern (string-upcase s) :keyword))
