(in-package #:eloquent.mvc.router)

(defclass <rule> ()
  ((action :documentation "Name of the function to handle a request"
           :initarg :action
           :reader rule-action
           :type symbol)
   (method :documentation "HTTP method supported by the action in this rule"
           :initarg :method
           :reader rule-method
           :type keyword)
   (uri-template :documentation "URI template of paths handled by the action in this rule"
                 :initarg :uri-template
                 :reader rule-uri-template
                 :type string))
  (:documentation "A rule for matching and hanlding a HTTP request"))

(defgeneric matchp (request rule)
  (:documentation "Return true when the request is match with rule, otherwise return false"))

(defmethod matchp ((request eloquent.mvc.request:<request>) (rule <rule>))
  (let ((request-method (eloquent.mvc.request:request-method request))
        (request-path-info (eloquent.mvc.request:request-path-info request)))
    (with-slots (method uri-template) rule
      (and (method= request-method method)
           (eloquent.mvc.prelude:equivalent request-path-info uri-template)))))

(defun method= (request-method rule-method)
  (eq request-method rule-method))
