(in-package :fw)

(defun assoc-string (item alist &key key (after #'cdr))
  (let ((c (assoc item alist :key key :test #'string=)))
    (if (or (null c) (null after))
        c
        (funcall after c))))

(defun get-current-ts ()
  (* 1000
     (local-time:timestamp-to-unix (local-time:now))))
