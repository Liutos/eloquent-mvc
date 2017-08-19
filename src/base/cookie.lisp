(in-package #:eloquent.mvc.base)

(defun parse-cookie-string (cookie-string)
  "Splits COOKIE-STRING, which is in format of Cookie, into an association list."
  (check-type cookie-string string)
  (decode-form-to-alist cookie-string #\; #\=
                        :trim-key-p t))
