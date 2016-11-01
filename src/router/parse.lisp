(in-package #:eloquent.mvc.router)

(defun components-to-rules (components)
  (flet ((aux (method uri-template action)
           (make-instance '<rule>
                          :action action
                          :method method
                          :uri-template uri-template)))
    (map-components #'aux components)))

(defun map-components (function components)
  (let ((result '()))
    (eloquent.mvc.prelude:dolist ((method uri-template action) components
                                  (nreverse result))
      (push (funcall function method uri-template action)
            result))))

(defun read-file-components (file)
  (with-open-file (stream file)
    (read stream)))

;;; EXPORT

(defun parse (file)
  "Read router rules from `file` and validate it."
  (declare (type (or pathname string) file))
  (let* ((components (read-file-components file))
         (rules (components-to-rules components)))
    (make-instance '<router>
                   :rules rules)))
