(in-package #:eloquent.mvc.router)

(defclass <router> ()
  ((rules :documentation "Routing rules in order"
          :initarg :rules
          :reader router-rules
          :type (trivial-types:proper-list <rule>)))
  (:documentation "Searching in routing rules by a request and provide a suitable handler function"))

(defun maprouter (function)
  "Apply FUNCTION to each rules in the router, and return a list of values returned by FUNCTION."
  (check-type function function)
  (let ((rules (router-rules *router*)))
    (mapcar function rules)))

(defvar *router* nil)
