(defpackage #:eloquent.mvc.router
  (:use #:cl)
  (:shadow :get)
  (:export #:get
           #:init
           #:maprouter
           #:path-info=
           #:print-rule
           #:rule-action
           #:show
           #:try-request))

(in-package #:eloquent.mvc.router)

(defun components-to-rules (components)
  (map-components #'make-rule components))

(defun find-rule (router request)
  (with-slots (rules) router
    (find-if #'(lambda (rule)
                 (matchp request rule))
             rules)))

(defun make-initargs (action)
  "Returns a list of keywords that are specified in ACTION explicitly, or extracted from the first element in ACTION(as a function)."
  (check-type action list)
  (destructuring-bind (action . initargs)
      action
    (when initargs
      (return-from make-initargs
        (mapcar #'alexandria:make-keyword initargs)))

    (let* ((arglist (trivial-arguments:arglist action))
           (keyword-params
            (nth-value 3
                       (alexandria:parse-ordinary-lambda-list arglist))))
      (mapcar #'caar keyword-params))))

(defun map-components (function components)
  (flet ((aux (component)
           (destructuring-bind (method uri-template action &rest args) component
             (apply function
                    method uri-template action
                    :allow-other-keys t args))))
    (mapcar #'aux components)))

(defun method= (request-method rule-method)
  (eq request-method rule-method))

(defun parse (file)
  "Read router rules from `file` and validate it."
  (check-type file (or pathname string))
  (let* ((rule-spec (read-rule-spec file))
         (rules (mapcar #'make-rule rule-spec)))
    (make-instance '<router>
                   :rules rules)))

(defun precondition-satisfy (precondition request)
  "Return T if PRECONDITION is null, or REQUEST satisfies the PRECONDITION."
  (check-type precondition (or function null))
  (check-type request eloquent.mvc.request:<request>)
  (or (null precondition)
      (funcall precondition request)))

(defun print-rule (rule)
  "Display the information of RULE in readable form."
  (check-type rule <rule>)
  (format t "~A ~A~%" (rule-method rule) (rule-uri-template rule)))

(defun read-rule-spec (file)
  "Read the content of FILE with customize reader macros."
  (let ((*readtable* (copy-readtable *readtable*)))
    (set-dispatch-macro-character #\# #\/ #'sharp-/ *readtable*)
    (with-open-file (stream file)
      (read stream))))

(defun sharp-/ (s c n)
  (declare (ignorable c n))
  (list :regexp (read s t (values) t)))

(defun slash-path-p (p)
  (alexandria:ends-with #\/ p))

(defun string=-but-last (long short)
  (string= long short :end2 (1- (length short))))

(defun template-keywords (template)
  "Extract placeholders from TEMPLATE and intern them as keyword symbols."
  (check-type template string)
  (mapcar #'(lambda (s)
              (eloquent.mvc.prelude:make-keyword (subseq s 1)))
          (cl-ppcre:all-matches-as-strings ":[-\\w_]+" template)))

(defun template-to-regexp (template)
  "Convert the TEMPLATE to equivalent regular expression."
  (check-type template string)
  (let ((regexp (cl-ppcre:regex-replace-all ":[-\\w_]+" template "([^/]+)")))
    (unless (char= (char regexp 0) #\^)
      (setf regexp (concatenate 'string "^" regexp)))
    (unless (char= (char regexp (1- (length regexp))) #\$)
      (setf regexp (concatenate 'string regexp "$")))
    regexp))

(defun make-rule-from-list (method uri-template action
                            &key
                              content-type
                              precondition
                              query-string-bind
                              (requestp t)
                              template)
  "Create and return a new router rule."
  (check-type method keyword)
  (check-type uri-template (or list string))
  (check-type action (or list symbol))
  (check-type precondition (or function null))
  (when (stringp uri-template)
    (setf uri-template (list :normal uri-template)))
  (setf action (alexandria:ensure-list action))
  (destructuring-bind (mode uri-template) uri-template
    (check-type mode keyword)
    (check-type uri-template string)
    (let ((action (first action))
          (initargs (make-initargs action)))
      (setf (cl:get action :content-type) content-type
            (cl:get action :initargs) initargs
            (cl:get action :query-string-bind) query-string-bind
            (cl:get action :requestp) requestp
            (cl:get action :template) template)
      (make-instance '<rule>
                     :action action
                     :initargs initargs
                     :method method
                     :mode mode
                     :precondition precondition
                     :uri-template uri-template))))

;;; export

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
   (precondition :documentation "A function designates the precondition of matching this rule."
                 :initarg :precondition
                 :reader rule-precondition
                 :type function)
   (uri-template :documentation "URI template of paths handled by the action in this rule"
                 :initarg :uri-template
                 :reader rule-uri-template
                 :type string))
  (:documentation "A rule for matching and hanlding a HTTP request"))

(defclass <router> ()
  ((rules :documentation "Routing rules in order"
          :initarg :rules
          :reader router-rules
          :type (trivial-types:proper-list <rule>)))
  (:documentation "Searching in routing rules by a request and provide a suitable handler function"))

(defvar *router* nil)

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
    (with-slots (method mode precondition uri-template) rule
      (and (method= request-method method)
           (path-info= mode request-path-info uri-template
                       :request request)
           (precondition-satisfy precondition request)))))

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

(defun get (request)
  "Return a symbol indicates the action function for REQUEST."
  (let ((rule (find-rule *router* request)))
    rule))

(defun init (file)
  "Parse the router configuration file."
  (setf *router* (parse file))
  (values))

(defun maprouter (function)
  "Apply FUNCTION to each rules in the router, and return a list of values returned by FUNCTION."
  (check-type function function)
  (let ((rules (router-rules *router*)))
    (mapcar function rules)))

(defun show (&optional (stream *standard-output*))
  "Print all routing rules to STREAM in readable form."
  (check-type stream stream)
  (flet ((aux (rule)
           (with-slots (action method uri-template) rule
             (format stream "~A~A~A ~S~%"
                     method #\Tab uri-template action))))
    (with-slots (rules) *router*
      (dolist (rule rules)
        (aux rule)))))

(defun try-request (method path)
  "Return a rule matching a request consists of METHOD and PATH."
  (check-type method keyword)
  (check-type path string)
  (let ((request (make-instance 'eloquent.mvc.request:<request>
                                :path-info path
                                :request-method method)))
    (find-rule *router* request)))
