(in-package #:eloquent.mvc.loader)

(defun start-server (config app)
  (let ((port (eloquent.mvc.config:get-server-port config)))
    (clack:clackup app
                   :port port)))

(defun stop-server (handler)
  (clack:stop handler))
