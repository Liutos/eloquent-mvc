(in-package #:eloquent.mvc.logger)

(defun last-hour ()
  "Return a string represents the last hour in format YYYYMMDDhhmm."
  (eloquent.mvc.prelude:format-timestring
   nil
   (local-time:timestamp- (local-time:universal-to-timestamp (get-universal-time))
                          1 :hour)
   :iso-8601-basic-date-format))

(defun make-destination (label log)
  "Return a pathname object represents the log file to be written.

The old log file would be renamed if it was created not at current hour."
  (let ((log-path (merge-pathnames (concatenate 'string (string-downcase (symbol-name label)) ".log")
                                   (log-directory log))))
    (rotate log-path)
    log-path))

(defun now ()
  "Return a string represents the current time, in the format YYYY-MM-DD hh:mm:ss."
  (local-time:format-timestring
   nil
   (local-time:universal-to-timestamp (get-universal-time))
   :format '((:year 2) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2) " " :gmt-offset-hhmm)))

(defun rotate (log-path)
  "Rename the file at LOG-PATH by appending the \".YYYYMMDDhh\" suffix if this log file was created in the last hour."
  (declare (optimize (speed 0)))
  (check-type log-path pathname)
  (unless (uiop:file-exists-p log-path)
    (return-from rotate))
  #+sbcl
  (let* ((stat (sb-posix:stat log-path))
         (ctime (sb-posix:stat-ctime stat))
         (now (eloquent.mvc.prelude:now :second))
         (current-hour (- now (mod now (* 60 60)))))
    (when (< ctime current-hour)
      (let* ((suffix (last-hour))
             (new-name (concatenate 'string
                                    (namestring log-path)
                                    "."
                                    suffix)))
        (rename-file log-path (pathname new-name))))))

;;; EXPORT

(defun format (label control-string &rest format-arguments)
  "Like function CL:FORMAT but output the formatted string into a file named by LABEL."
  (declare (type keyword label))
  (let ((destination (make-destination label *log*)))
    (with-open-file (stream destination
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (apply #'cl:format stream control-string format-arguments))))
