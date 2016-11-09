(in-package #:eloquent.mvc.dispatcher)

(defun call-action (action request)
  "Process REQUEST by calling ACTION.

The function stored in ACTION will be called by following arguments:
1. REQUEST. An instance of class ``eloquent.mvc.request:<request>'';
2. Placeholders extracted from URL by function ELOQUENT.MVC.ROUTER:PATH-INFO=;
3. Values extracted from query string by function PARSE-QUERY-STRING and STRING-ASSOC in package ELOQUENT.MVC.PRELUDE
The arguments above will be CONSed and passed to CL:APPLY for invoking."
  (check-type action symbol)
  (check-type request eloquent.mvc.request:<request>)
  (let (args
        (query-params (compute-query-params action request))
        (url-params (compute-url-params action request)))
    (setf args (append url-params query-params))
    (when (get action :requestp)
      (push request args))
    (apply (symbol-function action) args)))

(defun compute-query-params (action request)
  "Return a plist contains arguments extracted from query string of REQUEST to be passed as keyword arguments to ACTION."
  (check-type action symbol)
  (check-type request eloquent.mvc.request:<request>)
  (let* ((query-string (eloquent.mvc.request:request-query-string request))
         (query-params (eloquent.mvc.prelude:parse-query-string query-string)))
    (flet ((aux (binding)
           (destructuring-bind (var field) binding
             (list (alexandria:make-keyword var)
                   (eloquent.mvc.prelude:string-assoc field query-params)))))
      (alexandria:mappend #'aux (get action :query-string-bind)))))

(defun compute-url-params (action request)
  "Return a list contains arguments extracted from URL-PARAMS required by ACTION."
  (check-type action symbol)
  (check-type request eloquent.mvc.request:<request>)
  (let ((url-params (eloquent.mvc.request:getextra :url-params request)))
    (flet ((aux (key)
             (getf url-params key)))
      (mapcar #'aux (get action :initargs)))))

(defun make-action-caller ()
  (lambda (request)
    (let ((action (eloquent.mvc.router:get request)))
      (call-action action request))))

(defun make-middleware-caller (config middlewares action-caller)
  (declare (type (trivial-types:proper-list symbol) middlewares))
  (declare (type function action-caller))
  (flet ((make-next (middleware next)
           (declare (type symbol middleware))
           (lambda (request)
             (declare (type eloquent.mvc.request:<request> request))
             (funcall (symbol-function middleware) request next
                      :config config
                      :allow-other-keys t))))
    (reduce #'make-next
            middlewares
            :from-end t
            :initial-value action-caller)))

;;; EXPORT

(defun make-handler (config middlewares)
  "Return a newly created function for handling client request passed from function CLACK:CLACKUP."
  (let* ((action-caller (make-action-caller))
         (middleware-caller (make-middleware-caller config middlewares action-caller)))
    (lambda (env)
      (let ((request (eloquent.mvc.request:env-to-request env)))
        (eloquent.mvc.response:response-to-list
         (funcall middleware-caller request))))))
