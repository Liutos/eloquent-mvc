(in-package #:eloquent.mvc.router)

(defun components-to-rules (components)
  (map-components #'(lambda (method uri-template action)
                      (let ((action (eloquent.mvc.prelude:find-symbol* action))
                            (method (eloquent.mvc.prelude:make-keyword method)))
                        (make-instance '<rule>
                                     :action action
                                     :method method
                                     :uri-template uri-template)))
                  components))

(defun map-components (function components)
  (let ((result '()))
    (eloquent.mvc.prelude:dolist ((method uri-template action) components
                                  (nreverse result))
      (push (funcall function method uri-template action)
            result))))

(defun read-file-components (file)
  (let* ((text (eloquent.mvc.prelude:read-file-string file))
         (lines (eloquent.mvc.prelude:split text #\Newline
                                            :remove-empty-subseqs t)))
    (mapcar #'split-line lines)))

(defun split-line (line)
  (eloquent.mvc.prelude:split line #\Space
                              :remove-empty-subseqs t))

(defun parse (file)
  (let ((components (read-file-components file)))
    (make-instance '<router>
                   :rules (components-to-rules components))))
