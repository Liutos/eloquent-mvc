(in-package #:eloquent.mvc.prelude)

(defun urldecode (string)
  "Return the decoded form of STRING, if STRING is not well-formed, return the original content."
  (declare (type string string))
  (handler-case
      (do-urlencode:urldecode string)
    (do-urlencode:urlencode-malformed-string ()
      string)))
