(in-package #:eloquent.mvc.middleware)

(defun feed-static-file (path-info prefix root)
  (let ((path (remove-prefix prefix path-info)))
    (eloquent.mvc.response:respond
     (merge-pathnames path
                      (merge-pathnames "static/" root)))))

(defun get-prefix (config)
  (eloquent.mvc.config:get config "static-file" "prefix"))

(defun remove-prefix (prefix s)
  (subseq s (length prefix)))

(defun static-file (request next
                    &key config)
  (let ((path-info (eloquent.mvc.request:request-path-info request))
        (prefix (get-prefix config))
        (root (eloquent.mvc.config:get-application-root config)))
    (if (alexandria:starts-with-subseq prefix path-info)
        (feed-static-file path-info prefix root)
        (funcall next request))))
