(in-package :cl-user)

(defpackage fw-asd
  (:use :cl :asdf))

(in-package :fw-asd)

(defsystem fw
  :author "Liutos <mat.liutos@gmail.com>"
  :license "MIT"
  :depends-on (:alexandria
               :cl-ppcre
               :closer-mop
               :clack
               :clsql
               :http-body
               :jonathan
               :local-time
               :split-sequence)
  :components ((:file "handler" :depends-on ("package" "router"))
               (:file "http" :depends-on ("package"))
               (:file "mysql" :depends-on ("package" "util"))
               (:file "package")
               (:file "router" :depends-on ("http" "package"))
               (:file "serde" :depends-on ("package"))
               (:file "server" :depends-on ("http" "package" "router"))
               (:file "util" :depends-on ("package"))))
