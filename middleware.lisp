(in-package :fw)

(defun performance (env next)
  (let* ((begin-at (get-current-ts))
         (result (funcall next env))
         (end-at (get-current-ts)))
    (when (and (typep result '<http-response>)
               (listp (body-of result)))
      (setf (body-of result)
            (append (body-of result)
                    `(("performance"
                       ("beginAt" . ,begin-at)
                       ("endAt" . ,end-at)
                       ("total" . ,(- end-at begin-at)))))))
    result))

(defun response (env next)
  (let ((result (funcall next env)))
    (typecase result
      (<http-response>
       (unwrap result))
      (t result))))

(defun visitor (env next)
  (let ((result (funcall next env)))
    (when (and (typep result '<http-response>)
               (listp (body-of result)))
      (setf (body-of result)
            (append (body-of result)
                    `(("visitor"
                       ("ip" . ,(remote-addr-of env)))))))
    result))
