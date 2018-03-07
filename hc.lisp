(in-package :fw)

(defun http-request (uri &rest args &key jsonp &allow-other-keys)
  "封装DRAKMA:HTTP-REQUEST"
  (let* ((bytes (apply #'drakma:http-request uri `(,@args :allow-other-keys t)))
         (text (flexi-streams:octets-to-string
                bytes :external-format :utf-8))
         (result text))
    (when jsonp
      (setf result (jonathan:parse text :as jsonp)))
    result))
