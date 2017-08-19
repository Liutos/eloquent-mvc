(in-package #:eloquent.mvc.base)

(defun parse-query-string (query-string)
  "Split QUERY-STRING by delimiter #\\& and #\\=, turn it into an association-list."
  (check-type query-string (or null string))
  (if (null query-string)
      '()
      (let ((alist (decode-form-to-alist query-string #\& #\=)))
        (mapcar #'(lambda (pair)
                    (destructuring-bind (head . tail) pair
                      (cons head (urldecode tail))))
                alist))))

(defun urldecode (string)
  "Return the decoded form of STRING, if STRING is not well-formed, return the original content."
  (check-type string string)
  (handler-case
      (do-urlencode:urldecode string :queryp t)
    (do-urlencode:urlencode-malformed-string ()
      string)))

(defun urlencode (plain)
  "Return a string by encoding PLAIN in URL-encoding."
  (check-type plain string)
  (do-urlencode:urlencode plain :queryp t))
