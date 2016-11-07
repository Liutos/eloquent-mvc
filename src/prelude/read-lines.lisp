(in-package #:eloquent.mvc.prelude)

(defun read-lines (file)
  "Read the whole FILE and split it by newline character, removes the empty lines."
  (let ((text (read-file-string file)))
    (split text #\Newline
           :remove-empty-subseqs t)))
