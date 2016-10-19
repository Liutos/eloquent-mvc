(in-package #:eloquent.mvc.router)

(defclass <rule> ()
  ((action :documentation "Name of the function to handle a request"
           :initarg :action
           :reader rule-action
           :type symbol)
   (method :documentation "HTTP method supported by the action in this rule"
           :initarg :method
           :reader rule-method
           :type string)
   (uri-template :documentation "URI template of paths handled by the action in this rule"
                 :initarg :uri-template
                 :reader rule-uri-template
                 :type string))
  (:documentation "A rule for matching and hanlding a HTTP request"))
