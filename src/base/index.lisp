(in-package #:eloquent.mvc.prelude)

(define-condition alist-bind-error ()
  ((message
    :initarg :message
    :reader alist-bind-error-message
    :type string))
  (:report (lambda (c stream)
             (format stream "~A" (alist-bind-error-message c)))))

(defmacro alist-bind (bindings alist-expr &body body)
  "Bind variables to values extracted from ALIST, and evaluate the BODY."
  (alexandria:with-gensyms (alist)
    (labels ((wrap-value (expr &rest args)
               `(compute-value ,expr ,@args))
             (make-binding (binding)
               (destructuring-bind (var field &rest args) binding
                 `(,var ,(apply #'wrap-value
                                `(eloquent.mvc.base:alist-get ,alist ,field)
                                :path field
                                args)))))
      (let ((bindings (mapcar #'make-binding bindings)))
        `(let* ((,alist ,alist-expr)
                ,@bindings)
           ,@body)))))

(defun compute-value (raw
                      &key
                        default
                        path
                        requirep
                        type)
  "Validate and convert RAW to another form according to the keyword arguments."
  (when (and requirep (null raw))
    (error 'alist-bind-error
           :message (format nil "缺少`~A`参数" path)))
  (let ((value raw))
    (when type
      (ecase type
        (:integer (unless (integerp value)
                    (setf value (parse-integer value))))))
    (when (and default (null value))
      (setf value default))
    value))

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
