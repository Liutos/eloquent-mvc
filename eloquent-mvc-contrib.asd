(asdf:defsystem #:eloquent-mvc-contrib
  :version "0.0.1"
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (#:eloquent-mvc)
  :components
  ((:module "src"
            :components
            ((:module "contrib"
                      :components
                      ((:file "helper" :depends-on ("package"))
                       (:file "package")
                       (:module "middleware"
                                :components
                                ((:file "access-log")
                                 (:file "compress")
                                 (:file "fill-template")
                                 (:file "handle-error")
                                 (:file "not-found")
                                 (:file "parse-body"))
                                :depends-on ("package"))))))))
