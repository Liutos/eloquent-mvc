(in-package #:eloquent.mvc.logger)

(defun make-destination (label log)
  (merge-pathnames (concatenate 'string (string-downcase (symbol-name label)) ".log")
                   (log-directory log)))

(defun now ()
  "Return a string represents the current time, in the format YYYY-MM-DD hh:mm:ss."
  (local-time:format-timestring
   nil
   (local-time:universal-to-timestamp (get-universal-time))
   :format '((:year 2) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2) " " :gmt-offset-hhmm)))

;;; EXPORT

(defun format (label control-string &rest format-arguments)
  "Like function CL:FORMAT but output the formatted string into a file named by LABEL."
  (declare (type keyword label))
  (let ((destination (make-destination label *log*))
        (fmt (concatenate 'string "~A: " control-string)))
    (with-open-file (stream destination
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (apply #'cl:format stream fmt (now) format-arguments))))
