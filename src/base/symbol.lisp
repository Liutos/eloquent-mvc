(in-package #:eloquent.mvc.base)

(defun find-symbol* (s)
  "Parse S into a symbol with the same name and package."
  (check-type s string)
  (let ((l (split (string-upcase s) #\:
                  :remove-empty-subseqs t)))
    (optima:match l
      ((list name)
       (find-symbol name *package*))
      ((list package-name name)
       (find-symbol name package-name)))))

(defun make-keyword (s)
  "Intern the string S into package :KEYWORD, upcase it at first."
  (intern (string-upcase s) :keyword))
