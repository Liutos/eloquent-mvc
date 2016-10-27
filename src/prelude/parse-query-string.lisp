(in-package #:eloquent.mvc.prelude)

(defun parse-query-string (query-string)
  (if (null query-string)
      '()
      (let ((alist (eloquent.mvc.prelude:string-to-alist query-string #\& #\=)))
        (mapcar #'(lambda (pair)
                    (destructuring-bind (head . tail) pair
                      (cons head (urldecode tail))))
                alist))))
