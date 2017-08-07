(in-package #:eloquent.mvc.prelude.base64)

(defgeneric decode (input &key uri)
  (:documentation "Converts a base64-encoded INPUT into its original form. If URI is not NIL, INPUT will be considered as encoded by calling ENCODE with URI set to T also."))

(defgeneric encode (input &key uri)
  (:documentation "Converts INPUT into its base64-encoded form. If URI is not NIL, then the equal signs, plus signs and slash signs will be replaced to empty, minus signs and underscore signs."))

(defmethod decode ((input string) &key uri)
  "Converts a base64-encoded string to its original form."
  (flet ((right-pad ()
           (let* ((len (length input))
                  (diff (- (* 4 (ceiling (/ len 4)))
                           len)))
             (str:concat input (str:repeat diff "=")))))
    (setf input (right-pad))
    (when uri
      (str:replace-all "_" "/"
                       (str:replace-all "-" "+" input)))
    (cl-base64:base64-string-to-string input)))

(defmethod encode ((input string) &key uri)
  "Returns a base64-encoded string constructed from INPUT."
  (let ((encoded (cl-base64:string-to-base64-string input)))
    (if uri
        (str:replace-all "=" ""
                         (str:replace-all "+" "-"
                                          (str:replace-all "/" "_" encoded)))
        encoded)))
