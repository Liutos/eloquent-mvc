(in-package #:eloquent.mvc.response)

(defclass <response> ()
  ((body :documentation "The HTTP body"
         :initarg :body
         :reader response-body)
   (headers :documentation "The list of responding HTTP headers"
            :initarg :headers
            :reader response-headers
            :type (trivial-types:association-list))
   (status :documentation "The HTTP status code"
           :initarg :status
           :reader response-status
           :type fixnum))
  (:documentation "A HTTP response object"))
