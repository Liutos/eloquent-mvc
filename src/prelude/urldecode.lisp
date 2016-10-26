(in-package #:eloquent.mvc.prelude)

(defun urldecode (string)
  (declare (type string string))
  (handler-case
      (do-urlencode:urldecode string)
    (do-urlencode:urlencode-malformed-string ()
      string)))
