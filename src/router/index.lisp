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
    rule))

(defun init (file)
  "Parse the router configuration file."
  (setf *router* (parse file))
  (values))

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
