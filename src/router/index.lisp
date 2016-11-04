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
  (let ((result '()))
    (eloquent.mvc.prelude:dolist ((method uri-template action) components
                                  (nreverse result))
      (push (funcall function method uri-template action)
            result))))

(defun not-found (request)
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
  (with-open-file (stream file)
    (read stream)))

;;; EXPORT

(defun get (request)
  (let ((rule (find-rule *router* request)))
    (if rule
        (rule-action rule)
        'not-found)))

(defun init (file)
  (setf *router* (parse file))
  (values))
