(in-package #:eloquent.mvc.base)

(defun read-file-string (file)
  "Read the entire FILE into a single string."
  (uiop:read-file-string file))

(defun read-lines (file)
  "Read the whole FILE and split it by newline character, removes the empty lines."
  (let ((text (read-file-string file)))
    (split text #\Newline
           :remove-empty-subseqs t)))
