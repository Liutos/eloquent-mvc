(in-package #:eloquent.mvc.router)

(defclass <rule> ()
  ((action :documentation "Name of the function to handle a request"
           :initarg :action
           :reader rule-action
           :type symbol)
   (initargs
    :documentation "The arguments to be passed to action handler"
    :initarg :initargs
    :reader rule-initargs
    :type (trivial-types:proper-list keyword))
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

(defgeneric make-rule (rule-spec)
  (:documentation "Create an instance of ``<rule>'' according to RULE-SPEC."))
(defgeneric matchp (request rule)
  (:documentation "Return true when the request is match with rule, otherwise return false"))
(defgeneric path-info= (mode path-info uri-template &key request)
  (:documentation "Return true when PATH-INFO is match against URI-TEMPLATE according to MODE."))

(defmethod make-rule ((rule-spec list))
  "Create an instance of ``<rule>'' according to RULE-SPEC.

A RULE-SPEC of list form must contains at least three components: The HTTP method, a pattern of matching URL path, and the name of a function for processing this request. See the document of function ``make-rule-from-list'' for details."
  (apply #'make-rule-from-list rule-spec))

(defmethod matchp ((request eloquent.mvc.request:<request>) (rule <rule>))
  (let ((request-method (eloquent.mvc.request:request-method request))
        (request-path-info (eloquent.mvc.request:request-path-info request)))
    (with-slots (method mode uri-template) rule
      (and (method= request-method method)
           (path-info= mode request-path-info uri-template
                       :request request)))))

(defmethod path-info= ((mode (eql :normal)) (path-info string) (uri-template string)
                       &key request)
  "Return true when PATH-INFO is the same as URI-TEMPLATE except the last slash."
  (declare (ignorable request))
  (or (string= path-info uri-template)
      (and (slash-path-p path-info)
           (string=-but-last uri-template path-info))
      (and (slash-path-p uri-template)
           (string=-but-last path-info uri-template))))

(defmethod path-info= ((mode (eql :strict)) (path-info string) (uri-template string)
                       &key request)
  "Return true when PATH-INFO is the same as URI-TEMPLATE."
  (declare (ignorable request))
  (string= path-info uri-template))

(defmethod path-info= ((mode (eql :regexp)) (path-info string) (uri-template string)
                       &key request)
  "Return true when regular expression URI-TEMPLATE matches the PATH-INFO."
  (declare (ignorable request))
  (not (null (cl-ppcre:scan uri-template path-info))))

(defmethod path-info= ((mode (eql :template)) (path-info string) (uri-template string)
                       &key request)
  "Return true when PATH-INFO can be generated by filling URI-TEMPLATE."
  (declare (ignorable request))
  (let ((regexp (template-to-regexp uri-template)))
    (path-info= :regexp path-info regexp)))

(defmethod path-info= :around ((mode (eql :template)) (path-info string) (uri-template string)
                               &key request)
  "When the next PATH-INFO= returns true, extract keyword components from request's URL."
  (alexandria:when-let (matchp (call-next-method))
    (let ((keywords (template-keywords uri-template))
          (regexp (template-to-regexp uri-template)))
      (multiple-value-bind (match regs)
          (cl-ppcre:scan-to-strings regexp path-info)
        (declare (ignorable match))
        (let ((url-params (alexandria:mappend #'(lambda (k r)
                                                  (list k r))
                                              keywords (coerce regs 'list))))
          (setf (eloquent.mvc.request:getextra :url-params request)
                url-params)
          matchp)))))

(defun make-rule-from-list (method uri-template action
                            &key
                              query-string-bind
                              (requestp t))
  "Create and return a new router rule."
  (check-type method keyword)
  (check-type uri-template (or list string))
  (check-type action (or list symbol))
  (when (stringp uri-template)
    (setf uri-template (list :normal uri-template)))
  (setf action (alexandria:ensure-list action))
  (destructuring-bind (mode uri-template) uri-template
    (check-type mode keyword)
    (check-type uri-template string)
    (let ((action (first action))
          (initargs (mapcar #'alexandria:make-keyword (rest action))))
      (setf (cl:get action :initargs) initargs
            (cl:get action :query-string-bind) query-string-bind
            (cl:get action :requestp) requestp)
      (make-instance '<rule>
                     :action action
                     :initargs initargs
                     :method method
                     :mode mode
                     :uri-template uri-template))))

(defun method= (request-method rule-method)
  (eq request-method rule-method))

(defun slash-path-p (p)
  (alexandria:ends-with #\/ p))

(defun string=-but-last (long short)
  (string= long short :end2 (1- (length short))))

(defun template-keywords (template)
  "Extract placeholders from TEMPLATE and intern them as keyword symbols."
  (check-type template string)
  (mapcar #'(lambda (s)
              (eloquent.mvc.prelude:make-keyword (subseq s 1)))
          (cl-ppcre:all-matches-as-strings ":\\w+" template)))

(defun template-to-regexp (template)
  "Convert the TEMPLATE to equivalent regular expression."
  (check-type template string)
  (cl-ppcre:regex-replace-all ":\\w+" template "([^/]+)"))
