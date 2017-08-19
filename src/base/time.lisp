(in-package #:eloquent.mvc.base)

(defgeneric format-timestring (dest timestamp format
                               &optional timezone)
  (:documentation "Constructs a string representation of TIMESTAMP according to FORMAT and write to DEST."))

(defmethod format-timestring (dest (timestamp (eql :now)) format
                              &optional timezone)
  "Constructs a string represents current time in FORMAT, and writes it to DEST."
  (format-timestring dest (local-time:now) format timezone))

(defmethod format-timestring (dest (timestamp integer) format
                              &optional timezone)
  "Writes a string represents TIMESTAMP in FORMAT to DEST. The TIMESTAMP is the milliseconds from Epoch time."
  (let ((seconds (truncate timestamp 1000)))
    (format-timestring dest (local-time:unix-to-timestamp seconds) format timezone)))

(defmethod format-timestring (dest timestamp format
                              &optional timezone)
  "Behave as LOCAL-TIME:FORMAT-TIMESTRING."
  (local-time:format-timestring dest timestamp :format format :timezone timezone))

(defmethod format-timestring (dest timestamp (format (eql :iso-8601-basic-date-format))
                              &optional timezone)
  "Constructs a string representation of TIMESTAMP in the basic ISO-8601 format, and writes it to DEST. Only the date parts will be output, and time, includes hours, minutes and seconds will be ignored."
  (let ((format '((:year 2) (:month 2) (:day 2) (:hour 2))))
    (local-time:format-timestring dest timestamp :format format :timezone timezone)))

(defmethod format-timestring (dest timestamp (format (eql :iso-8601-bj-format))
                              &optional timezone)
  "Constructs a string representation of TIMESTAMP in the ISO-8601 like format, and writes to DEST. The optional parameter TIMEZONE is ignored and the underlying timezone will be set to Asia/Shanghai.

The concrete output format is YYYY-MM-DD hh:mm:ss."
  (declare (ignorable timezone))
  (local-time:reread-timezone-repository)
  (let ((timezone (local-time:find-timezone-by-location-name "Asia/Shanghai")))
    (local-time:format-timestring
     dest timestamp
     :format '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2))
     :timezone timezone)))

(defmethod format-timestring (dest timestamp (format (eql :nginx-log-format))
                              &optional timezone)
  "Constructs a string representation of TIMESTAMP in format like Nginx log timestamp, and writes it to DEST."
  (let ((format '((:day 2) "/" :short-month "/" (:year 2) ":" (:hour 2) ":" (:min 2) ":" (:sec 2) " " :gmt-offset-hhmm)))
    (local-time:format-timestring dest timestamp :format format :timezone timezone)))

(defgeneric now (precision)
  (:documentation "Returns the current time interval in second, millisecond, or microsecond precision."))

(defmethod now ((precision (eql :second)))
  "Returns the number of seconds elapsed since 1 January 1970 00:00:00 UTC."
  (local-time:timestamp-to-unix (local-time:now)))

(defmethod now ((precision (eql :millisecond)))
  "Returns the number of milliseconds elapsed since 1 January 1970 00:00:00 UTC."
  (let* ((now (local-time:now))
         (seconds (local-time:timestamp-to-unix now))
         (milliseconds (local-time:timestamp-millisecond now)))
    (+ (* 1000 seconds) milliseconds)))

(defmethod now ((precision (eql :microsecond)))
  "Returns the number of microseconds elapsed since 1 January 1970 00:00:00 UTC."
  (let* ((now (local-time:now))
         (seconds (local-time:timestamp-to-unix now))
         (microsecond (nth-value 1 (local-time:timestamp-millisecond now))))
    (+ (* 1000000 seconds) microsecond)))
