(in-package #:eloquent.mvc.response)

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
           (setf bytes-sent 0)))))

(defmethod (setf response-body) :after ((body sequence) (object <response>))
  (setf (slot-value object 'bytes-sent) (length body)))
