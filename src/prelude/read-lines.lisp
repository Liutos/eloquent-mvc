(in-package #:eloquent.mvc.prelude)

(defun read-lines (file)
  (let ((text (read-file-string file)))
    (split text #\Newline
           :remove-empty-subseqs t)))
