(in-package :fw)

(defclass <http-response> ()
  ((body
    :accessor body-of
    :initarg :body
    :documentation "响应的数据")
   (code
    :accessor code-of
    :initarg :code
    :type integer
    :documentation "响应的状态码")
   (content-type
    :accessor content-type-of
    :initarg :content-type
    :initform nil
    :type string
    :documentation "响应内容的类型")
   (headers
    :accessor headers-of
    :initarg :headers
    :type list
    :documentation "响应的一系列头部")
   (to-json-from
    :accessor to-json-from-of
    :initarg :to-json-from
    :initform nil
    :type keyword
    :documentation "序列化为JSON字符串时来源数据的格式"))
  (:documentation "HTTP响应"))

(defmacro http-let (bindings (env &key (parser 'http-body:parse)) &body forms)
  "HTTP-LET ({(var &key default from key type)| var}*) (env &key parser) forms

During evaluation of the FORMS, bind the VARs to the result of querying KEY in the FROM part of HTTP request. The result should be type TYPE or be converted to. If the KEY is not found, the DEFAULT will be the replacement.

There are three valid values for FROM:

1. :BODY. It means the KEY should be searched within HTTP body. The raw HTTP body will be parsed by PARSER. A default parser based on HTTP-BODY:PARSE is provided;
2. :QS. It means the KEY should be searched within query string;
3. :URL. It means the KEY should be searched within URL. The KEY used as index for getting one of the matching groups."
  (alexandria:with-gensyms (body qs url-groups)
    (alexandria:once-only (env)
      `(let ((,body (funcall 'http-let/parse-body ,env ',parser))
             (,qs (funcall 'http-let/parse-qs ,env))
             (,url-groups (getf ,env :params)))
         (let ,(mapcar #'(lambda (binding)
                           (destructuring-bind (var . args) binding
                             `(,var (apply 'http-let/parse-binding ,body ,qs ,url-groups ,@args nil))))
                       bindings)
           ,@forms)))))

(defun content-length-of (env)
  (getf env :content-length))

(defmethod content-type-of ((env list))
  (getf env :content-type))

(defmethod headers-of ((env list))
  (getf env :headers))

(defun method-of (env)
  (getf env :request-method))

(defun path-of (env)
  (getf env :path-info))

(defun query-string-of (env)
  (getf env :query-string))

(defun raw-body-of (env)
  (getf env :raw-body))

(defun remote-addr-of (env)
  (getf env :remote-addr))

(defun http-let/parse-binding (body qs url-params &rest args &key default from key (trimp t) type)
  (declare (ignorable key))
  (flet ((get-from-body (&key key)
           (assoc-string key body))
         (get-from-qs (&key key)
           (assoc-string key qs))
         (get-from-url (&key key)
           (assoc-string key url-params)))
    (let ((getter (ecase from
                    (:body #'get-from-body)
                    (:qs #'get-from-qs)
                    (:url #'get-from-url))))
      (let ((result (apply getter :allow-other-keys t args)))
        (when (and (null result) default)
          (setf result default))
        (when (and trimp (stringp result))
          (setf result (string-trim '(#\Newline #\Space) result)))
        (when (and (eq type :integer)
                   (stringp result))
          (setf result (parse-integer result)))
        result))))

(defun http-let/parse-body (env parser)
  (let ((content-length (content-length-of env))
        (content-type (content-type-of env))
        (raw-body (raw-body-of env)))
    (funcall parser content-type content-length raw-body)))

(defun http-let/parse-qs (env)
  (let ((src (query-string-of env)))
    (when (or (null src) (string= src ""))
      (return-from http-let/parse-qs nil))

    (let ((pairs (split-sequence:split-sequence #\& src)))
      (mapcar #'(lambda (pair)
                  (destructuring-bind (k v)
                      (split-sequence:split-sequence #\= pair)
                    (cons k v)))
              pairs))))

(defun make-http-response (body code headers)
  (make-instance '<http-response>
                 :headers headers
                 :code code
                 :body body))

(defun respond-as-json (value &key (from :alist) (code 200) headers)
  (let ((resp (make-http-response value code headers)))
    (setf (content-type-of resp) "application/json")
    (setf (to-json-from-of resp) from)
    resp))

(defun unwrap (response)
  (check-type response <http-response>)
  (with-slots (body code content-type headers)
      response
    (when (string= content-type "application/json")
      (setf body (jonathan:to-json body
                                   :from (to-json-from-of response)))
      (setf headers (append headers `(:content-type ,content-type))))
    (list code
          headers
          (list body))))
