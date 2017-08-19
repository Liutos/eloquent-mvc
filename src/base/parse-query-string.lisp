(in-package #:eloquent.mvc.prelude)

(defun parse-query-string (query-string)
  "Split QUERY-STRING by delimiter #\\& and #\\=, turn it into an association-list."
  (if (null query-string)
      '()
      (let ((alist (eloquent.mvc.prelude:string-to-alist query-string #\& #\=)))
        (mapcar #'(lambda (pair)
                    (destructuring-bind (head . tail) pair
                      (cons head (urldecode tail))))
                alist))))
