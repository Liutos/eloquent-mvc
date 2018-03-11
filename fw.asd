(in-package :cl-user)

(defpackage fw-asd
  (:use :cl :asdf))

(in-package :fw-asd)

(defsystem fw
  :author "Liutos <mat.liutos@gmail.com>"
  :license "MIT"
  :depends-on (:alexandria
               :cl-annot
               :cl-ppcre
               :closer-mop
               :clack
               :clsql
               :drakma
               :flexi-streams
               :http-body
               :jonathan
               :ironclad
               :local-time
               :split-sequence
               :str)
  :components ((:file "digest" :depends-on ("package"))
               (:file "handler" :depends-on ("package" "router"))
               (:file "hc" :depends-on ("package"))
               (:file "http" :depends-on ("package"))
               (:file "memoize" :depends-on ("package" "util"))
               (:file "mysql" :depends-on ("package" "util"))
               (:file "package")
               (:file "router" :depends-on ("http" "package"))
               (:file "serde" :depends-on ("package"))
               (:file "server" :depends-on ("http" "package" "router"))
               (:file "util" :depends-on ("package"))))
