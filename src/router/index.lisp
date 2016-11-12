(in-package #:eloquent.mvc.router)

(defvar *router* nil)

(defun components-to-rules (components)
  (map-components #'make-rule components))

(defun find-rule (router request)
  (with-slots (rules) router
    (find-if #'(lambda (rule)
                 (matchp request rule))
             rules)))

(defun map-components (function components)
  (flet ((aux (component)
           (destructuring-bind (method uri-template action &rest args) component
             (apply function
                    method uri-template action
                    :allow-other-keys t args))))
    (mapcar #'aux components)))

(defun not-found (request)
  "Send a response with HTTP code 404 to client."
  (declare (ignore request))
  (eloquent.mvc.response:respond
   ""
   :status 404))

(defun parse (file)
  "Read router rules from `file` and validate it."
  (check-type file (or pathname string))
  (let* ((rule-spec (read-rule-spec file))
         (rules (mapcar #'make-rule rule-spec)))
    (make-instance '<router>
                   :rules rules)))

(defun read-rule-spec (file)
  "Read the content of FILE with customize reader macros."
  (let ((*readtable* (copy-readtable *readtable*)))
    (set-dispatch-macro-character #\# #\/ #'sharp-/ *readtable*)
    (with-open-file (stream file)
      (read stream))))

(defun sharp-/ (s c n)
  (declare (ignorable c n))
  (list :regexp (read s t (values) t)))

;;; EXPORT

(defun get (request)
  "Return a symbol indicates the action function for REQUEST."
  (let ((rule (find-rule *router* request)))
    (cond (rule
           (rule-action rule))
          (t 'not-found))))

(defun init (file)
  "Parse the router configuration file."
  (setf *router* (parse file))
  (values))

(defun try-request (method path)
  "Return a rule matching a request consists of METHOD and PATH."
  (check-type method keyword)
  (check-type path string)
  (let ((request (make-instance 'eloquent.mvc.request:<request>
                                :path-info path
                                :request-method method)))
    (find-rule *router* request)))
