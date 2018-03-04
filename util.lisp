(in-package :fw)

(defun assoc-string (item alist &key key (after #'cdr))
  (let ((c (assoc item alist :key key :test #'string=)))
    (if (or (null c) (null after))
        c
        (funcall after c))))

(defun get-current-ts ()
  (let* ((now (local-time:now))
         (unix-part (local-time:timestamp-to-unix now))
         (nsec-part (local-time:nsec-of now)))
    (+ (* 1000 unix-part)
       (truncate nsec-part 1000000))))

(defun my-getf (place indicator &optional default)
  (let ((i (position indicator place)))
    (if (numberp i)
        (nth (1+ i) place)
        default)))
