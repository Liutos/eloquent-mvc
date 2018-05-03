(in-package :cl-user)

(defpackage fw
  (:use :cl)
  (:export #:connect-to-mysql
           #:make-snowflake-id))
