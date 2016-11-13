(in-package #:eloquent.mvc.middleware)

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
    (when (get action :requestp t)
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

;;; EXPORT

(defun apply-matched-rule (request next &key)
  "Call the real user-defined action stored in REQUEST."
  (declare (ignorable next))
  (check-type request eloquent.mvc.request:<request>)
  (let ((action (eloquent.mvc.router::rule-action
                 (eloquent.mvc.request:getextra :matched-rule request))))
    (call-action action request)))
