(in-package #:eloquent.mvc.loader)

(defun start-server (config app)
  (let ((port (eloquent.mvc.config:get-server-port config))
        (server (eloquent.mvc.config:get-server-server config)))
    (clack:clackup app
                   :port port
                   :server server)))

(defun stop-server (handler)
  (clack:stop handler))
