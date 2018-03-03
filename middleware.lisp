(in-package :fw)

(defun visitor (env next)
  (format t "The income IP is ~A~%" (remote-addr-of env))
  (funcall next env))
