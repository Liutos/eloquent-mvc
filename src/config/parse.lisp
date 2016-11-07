(in-package #:eloquent.mvc.config)

(defun parse (filename)
  "Read content from a file specified by FILENAME, parse it and return the parsing result."
  (declare (type pathname filename))
  (let ((content (cl-yaml:parse filename)))
    (make-instance '<config>
                   :content content)))
