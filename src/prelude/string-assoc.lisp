(in-package #:eloquent.mvc.prelude)

(defun string-assoc (item alist
                     &rest args
                     &key
                       (after #'cdr)
                       key
                       test
                       test-not)
  "Like function CL:ASSOC but use CL:STRING= for testing in default, and return the CDR of result found by CL:ASSOC"
  (declare (ignorable key test test-not))
  (when (and (null test-not) (null test))
    (push #'string= args)
    (push :test args))
  (let ((val (apply #'assoc item alist :allow-other-keys t args)))
    (when (and after val)
      (funcall after val))))
