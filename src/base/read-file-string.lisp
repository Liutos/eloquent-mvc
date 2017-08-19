(in-package #:eloquent.mvc.prelude)

(defun read-file-string (file)
  "Read the entire FILE into a single string."
  (uiop:read-file-string file))
