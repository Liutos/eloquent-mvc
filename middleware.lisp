(in-package :fw)

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
