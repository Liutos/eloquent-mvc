(in-package #:eloquent.mvc.prelude)

(defun find-symbol* (s)
  "Parse S into a symbol with the same name and package."
  (declare (type string s))
  (let ((l (eloquent.mvc.prelude:split (string-upcase s) #\:
                                       :remove-empty-subseqs t)))
    (optima:match l
      ((list name)
       (find-symbol name *package*))
      ((list package-name name)
       (find-symbol name package-name)))))
