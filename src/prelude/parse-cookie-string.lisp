(in-package #:eloquent.mvc.prelude)

(defun parse-cookie-string (cookie-string)
  "Splits COOKIE-STRING, which is in format of Cookie, into an association list."
  (string-to-alist cookie-string #\; #\=
                   :trim-key-p t))
