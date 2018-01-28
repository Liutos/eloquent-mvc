(in-package :fw)

(defmacro http-let (bindings (env &key (parser 'http-body:parse) url-pattern) &body forms)
  "HTTP-LET ({(var &key default from key type)| var}*) (env &key parser url-pattern) forms

During evaluation of the FORMS, bind the VARs to the result of querying KEY in the FROM part of HTTP request. The result should be type TYPE or be converted to. If the KEY is not found, the DEFAULT will be the replacement.

There are three valid values for FROM:

1. :BODY. It means the KEY should be searched within HTTP body. The raw HTTP body will be parsed by PARSER. A default parser based on HTTP-BODY:PARSE is provided;
2. :QS. It means the KEY should be searched within query string;
3. :URL. It means the KEY should be searched within URL. The URL will be match against URL-PATTERN, and KEY used as index for getting one of the matching groups."
  (alexandria:with-gensyms (body qs url-groups)
    (alexandria:once-only (env)
      `(let ((,body (funcall 'http-let/parse-body ,env ',parser))
             (,qs (funcall 'http-let/parse-qs ,env))
             (,url-groups (funcall 'http-let/parse-url ,env ,url-pattern)))
         (let ,(mapcar #'(lambda (binding)
                           (destructuring-bind (var . args) binding
                             `(,var (apply 'http-let/parse-binding ,body ,qs ,url-groups ,@args nil))))
                       bindings)
           ,@forms)))))

(defun content-length-of (env)
  (getf env :content-length))

(defun content-type-of (env)
  (getf env :content-type))

(defun headers-of (env)
  (getf env :headers))

(defun method-of (env)
  (getf env :request-method))

(defun path-of (env)
  (getf env :path-info))

(defun query-string-of (env)
  (getf env :query-string))

(defun raw-body-of (env)
  (getf env :raw-body))

(defun http-let/parse-binding (body qs url-groups &rest args &key default from index key (trimp t) type)
  (declare (ignorable index key))
  (flet ((get-from-body (&key key)
           (assoc-string key body))
         (get-from-qs (&key key)
           (assoc-string key qs))
         (get-from-url (&key index)
           (aref url-groups index)))
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

(defun http-let/parse-url (env url-pattern)
  (unless url-pattern
    (return-from http-let/parse-url nil))
  (let ((path (path-of env)))
    (nth-value 1
               (cl-ppcre:scan-to-strings url-pattern path))))

(defun respond-as-json (code headers value)
  (list
   code
   (append headers '(:content-type "application/json"))
   (list (jonathan:to-json value :from :alist))))
