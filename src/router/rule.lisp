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
   (mode :documentation "A mode instructs how to compare a path-info and a uri-template"
         :initarg :mode
         :reader rule-mode
         :type keyword)
   (uri-template :documentation "URI template of paths handled by the action in this rule"
                 :initarg :uri-template
                 :reader rule-uri-template
                 :type string))
  (:documentation "A rule for matching and hanlding a HTTP request"))

(defgeneric matchp (request rule)
  (:documentation "Return true when the request is match with rule, otherwise return false"))
(defgeneric path-info= (mode path-info uri-template)
  (:documentation "Return true when PATH-INFO is match against URI-TEMPLATE according to MODE."))

(defmethod matchp ((request eloquent.mvc.request:<request>) (rule <rule>))
  (let ((request-method (eloquent.mvc.request:request-method request))
        (request-path-info (eloquent.mvc.request:request-path-info request)))
    (with-slots (method mode uri-template) rule
      (and (method= request-method method)
           (path-info= mode request-path-info uri-template)))))

(defmethod path-info= ((mode (eql :normal)) (path-info string) (uri-template string))
  "Return true when PATH-INFO is the same as URI-TEMPLATE except the last slash."
  (or (string= path-info uri-template)
      (and (slash-path-p path-info)
           (string=-but-last uri-template path-info))
      (and (slash-path-p uri-template)
           (string=-but-last path-info uri-template))))

(defmethod path-info= ((mode (eql :strict)) (path-info string) (uri-template string))
  "Return true when PATH-INFO is the same as URI-TEMPLATE."
  (string= path-info uri-template))

(defmethod path-info= ((mode (eql :regexp)) (path-info string) (uri-template string))
  "Return true when regular expression URI-TEMPLATE matches the PATH-INFO."
  (not (null (cl-ppcre:scan uri-template path-info))))

(defun make-rule (method uri-template action)
  "Create and return a new router rule."
  (check-type method keyword)
  (check-type uri-template (or list string))
  (check-type action symbol)
  (when (stringp uri-template)
    (setf uri-template (list :normal uri-template)))
  (destructuring-bind (mode uri-template) uri-template
    (check-type mode keyword)
    (check-type uri-template string)
    (make-instance '<rule>
                   :action action
                   :method method
                   :mode mode
                   :uri-template uri-template)))

(defun method= (request-method rule-method)
  (eq request-method rule-method))

(defun slash-path-p (p)
  (alexandria:ends-with #\/ p))

(defun string=-but-last (long short)
  (string= long short :end2 (1- (length short))))
