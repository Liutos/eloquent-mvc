(in-package #:eloquent.mvc.request)

(defun get-cookie (request field)
  "Returns the value of FIELD in REQUEST's Cookie."
  (check-type field string)
  (check-type request <request>)
  (let ((cookie-string (get-header request "cookie")))
    (when (null cookie-string)
      (return-from get-cookie nil))
    (let ((cookie (eloquent.mvc.prelude:parse-cookie-string cookie-string)))
      (eloquent.mvc.base:alist-get cookie field))))
