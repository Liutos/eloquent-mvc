(defpackage #:eloquent.mvc.response
  (:use #:cl)
  (:export #:<response>
           #:http-compatible-error
           ;; accessor
           #:http-message
           #:http-status
           #:response-body
           #:response-bytes-sent
           #:response-headers
           #:response-status

           #:append-header
           #:make-encoder
           #:override-header
           #:respond
           #:respond-json
           #:response-to-list))

(in-package #:eloquent.mvc.response)

(defun make-headers (response)
  (with-slots (bytes-sent headers) response
    (override-header headers :content-length bytes-sent)))

;;; export

(defclass <response> ()
  ((body :documentation "The HTTP body"
         :initarg :body
         :accessor response-body)
   (bytes-sent :documentation "The number of bytes send back to client"
               :reader response-bytes-sent
               :type fixnum)
   (headers :documentation "The list of responding HTTP headers"
            :initarg :headers
            :accessor response-headers
            :type (trivial-types:proper-list (trivial-types:tuple keyword t)))
   (status :documentation "The HTTP status code"
           :initarg :status
           :reader response-status
           :type fixnum))
  (:documentation "A HTTP response object"))

(define-condition http-compatible-error (error)
  ((message
    :initarg :message
    :reader http-message
    :type string)
   (status
    :initarg :status
    :reader http-status
    :type fixnum))
  (:report (lambda (c stream)
             (format stream "~D:~A" (http-status c) (http-message c)))))

(defmethod initialize-instance :after ((instance <response>) &rest initargs)
  (declare (ignore initargs))
  (with-slots (body bytes-sent) instance
    (cond ((stringp body)
           (setf bytes-sent (flexi-streams:octet-length body
                                                        :external-format :utf-8)))
          ((pathnamep body)
           (setf bytes-sent
                 (with-open-file (stream body
                                         :element-type '(unsigned-byte 8))
                   (file-length stream))))
          ((null body)
           (setf bytes-sent 0))
          ((vectorp body)
           (setf bytes-sent (length body))))))

(defmethod (setf response-body) :after ((body sequence) (object <response>))
  (setf (slot-value object 'bytes-sent) (length body)))

(defgeneric make-encoder (encoder)
  (:documentation "Returns a function for encoding a value into a string."))

(defmethod make-encoder ((encoder (eql :json-plist)))
  "Returns a function for encoding a PLIST into a JSON string."
  #'cl-json:encode-json-plist-to-string)

(defmethod make-encoder ((encoder (eql :json-plist-list)))
  "Returns a function for encoding a list consist of plist into a JSON string."
  (lambda (list)
    (check-type list list)
    (with-output-to-string (s)
      (json:with-array (s)
        (loop
           :for rest :on list
           :do (json:encode-json-plist (first rest) s)
           :when (rest rest) :do (princ "," s))))))

(defun append-header (response field value)
  "Add a new HTTP header, whose field name is FIELD and value is VALUE, to the existing headers in RESPONSE."
  (check-type response <response>)
  (check-type field keyword)
  (check-type value string)
  (with-slots (headers) response
    (setf headers (override-header headers field value))))

(defun response-to-list (response)
  (declare (type <response> response))
  (with-slots (body headers status) response
    (let ((body (if (stringp body)
                    (list body)
                    body))
          (headers (make-headers response)))
      `(,status
        ,headers
        ,body))))

(defun override-header (headers field value)
  "Return a new plist contains all elements in HEADERS except the FIELD key. The value of FIELD in the newly created plist is replaced by VALUE."
  (declare (type (trivial-types:property-list) headers))
  (declare (type keyword field))
  (let ((filtered (alexandria:remove-from-plist headers field)))
    (cons field (cons value filtered))))

(defun respond (body
                &key
                  (encoder #'identity)
                  (headers '())
                  (status 200))
  (when (keywordp encoder)
    (setf encoder (make-encoder encoder)))
  (make-instance '<response>
                 :body (funcall encoder body)
                 :status status
                 :headers headers))

(defun respond-json (body
                     &key
                       (encoder #'cl-json:encode-json-to-string)
                       (headers '())
                       (status 200))
  "Encoding body into JSON and send response with at least the header Content-Type equals \"application/json\""
  (setf headers
        (override-header headers :content-type "application/json"))
  (respond
   body
   :encoder encoder
   :headers headers
   :status status))
