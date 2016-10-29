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
           (path-info= request-path-info uri-template)))))

(defun method= (request-method rule-method)
  (eq request-method rule-method))

(defun path-info= (request-path-info uri-template)
  (declare (type string request-path-info uri-template))
  (let ((uri-template (cl-ppcre:parse-string uri-template)))
    (etypecase uri-template
      (character (string= request-path-info (string uri-template)))
      (list (cl-ppcre:scan uri-template request-path-info))
      (string (or (string= request-path-info uri-template)
                  (and (slash-path-p request-path-info)
                       (string=-but-last uri-template request-path-info))
                  (and (slash-path-p uri-template)
                       (string=-but-last request-path-info uri-template)))))))

(defun slash-path-p (p)
  (alexandria:ends-with #\/ p))

(defun string=-but-last (long short)
  (string= long short :end2 (1- (length short))))
