(in-package #:eloquent.mvc.router)

(defclass <router> ()
  ((rules :documentation "Routing rules in order"
          :initarg :rules
          :reader router-rules
          :type (trivial-types:proper-list <rule>)))
  (:documentation "Searching in routing rules by a request and provide a suitable handler function"))
