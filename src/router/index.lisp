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
           (apply function component)))
    (mapcar #'aux components)))

(defun not-found (request)
  "Send a response with HTTP code 404 to client."
  (declare (ignore request))
  (eloquent.mvc.response:respond
   ""
   :status 404))

(defun parse (file)
  "Read router rules from `file` and validate it."
  (declare (type (or pathname string) file))
  (let* ((components (read-file-components file))
         (rules (components-to-rules components)))
    (make-instance '<router>
                   :rules rules)))

(defun read-file-components (file)
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
