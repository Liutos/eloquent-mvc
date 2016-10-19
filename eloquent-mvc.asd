(asdf:defsystem #:eloquent-mvc
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (#:alexandria
               #:clack
               #:split-sequence
               #:trivial-types
               #:uiop)
  :components ((:module "src"
                        :serial t
                        :components ((:module "prelude"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "dolist")
                                                           (:file "equivalent")
                                                           (:file "read-file-string")
                                                           (:file "split")))
                                     (:module "request"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "request")))
                                     (:module "router"
                                              :serial t
                                              :components ((:file "package")
                                                           (:file "rule")
                                                           (:file "router")
                                                           (:file "get")
                                                           (:file "parse")))
                                     (:file "package")))))
